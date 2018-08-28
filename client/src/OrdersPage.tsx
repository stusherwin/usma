import * as React from 'react';

import { CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
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
        <h1>Orders</h1>
        {!currentOrder? <Link action={this.newOrder}>New order</Link> : (
          <div>
            <h2>Current order</h2>
            <div>
              <div>
                <span>{ Util.formatDate(currentOrder.createdDate) }</span>
                <span>Status: {currentOrder.status}</span>
                <Money amount={currentOrder.total} />
                <RouterLink path={`/orders/${currentOrder.id}`}>View</RouterLink>
              </div>
            </div>
          </div>
        )}
        <h2>Past orders</h2>
        {!pastOrders.length ? <div>No past orders</div> : (
          <div>
            { pastOrders.map(o => (
              <div key={o.id}>
                <span>{ Util.formatDate(o.createdDate) }</span>
                <span>{o.status}</span>
                <Money amount={o.total} />
                <RouterLink path={`/orders/${o.id}`}>View</RouterLink>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}