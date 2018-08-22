import * as React from 'react';

import { CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrdersPageProps { orders: CollectiveOrder[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , navigate: (location: string) => void
                                 , reload: () => void
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, {}> {
  newOrder = () => {
    this.props.request(ServerApi.command.createOrder())
      .then(id => this.props.navigate(`/orders/${id}`))
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
                <Money amount={currentOrder.total} />
                <Link action={_ => this.props.navigate(`/orders/${currentOrder.id}`)}>View</Link>
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
                <span>{o.isCancelled && 'cancelled'}</span>
                <Money amount={o.total} />
                <Link action={_ => this.props.navigate(`/orders/${o.id}`)}>View</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}