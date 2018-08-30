import * as React from 'react';

import { CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'

export interface OrdersPageProps { orders: CollectiveOrder[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , reload: () => Promise<void>
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, {}> {
  newOrder = () => {
    this.props.request(ServerApi.command.createOrder())
      .then(id => this.props.reload()
                    .then(_ => Router.navigate(`/orders/${id}`)))
  }

  render() {
    const currentOrder = this.props.orders.filter(o => !o.isPast && !o.isCancelled)[0]
    const pastOrders = this.props.orders.filter(o => o.isPast || o.isCancelled)
    return (
      <div>
        <div className="bg-img-order bg-no-repeat bg-16 pl-16 min-h-16">
          <h1>Orders</h1>
        </div>
        {!currentOrder? <Button action={this.newOrder}>New order</Button> : (
          <div>
            <h2>Current order</h2>
            <div>
              <div>
                <RouterLink path={`/orders/${currentOrder.id}`}>{Util.formatDate(currentOrder.createdDate)}</RouterLink>
                <span>{currentOrder.status}</span>
                <Money amount={currentOrder.total} />
              </div>
            </div>
          </div>
        )}
        <h2>Past orders</h2>
        {!pastOrders.length ? <div>No past orders</div> : (
          <div>
            { pastOrders.map(o => (
              <div key={o.id}>
                <RouterLink path={`/orders/${o.id}`}>{ Util.formatDate(o.createdDate)}</RouterLink>
                <span>{o.status}</span>
                <Money amount={o.total} />
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}