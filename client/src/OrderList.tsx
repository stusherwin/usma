import * as React from 'react';

import { Order } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrderListProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export class OrderList extends React.Component<OrderListProps, { orders: Order[], initialised: boolean }> {
  constructor(props: OrderListProps) {
    super(props)

    this.state = { orders: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getOrderList())
      .then(orders => {
        this.setState({ orders
                      , initialised: true
                      })
      })
  }

  newOrder = () => {
    this.props.request(ServerApi.newOrder()).then(id => this.props.navigate('/orders/' + id))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    const currentOrder = this.state.orders.filter(o => !o.complete)[0]
    const pastOrders = this.state.orders.filter(o => o.complete)
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
                <Link action={_ => this.props.navigate('/orders/' + currentOrder.id)}>Manage</Link>
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
                <Money amount={o.total} />
                <Link action={_ => this.props.navigate('/orders/' + o.id)}>View</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}