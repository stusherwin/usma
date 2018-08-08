import * as React from 'react';

import { OrderSummary } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrderPageProps { id: number
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export interface OrderPageState { order: OrderSummary | null
                                   , initialised: boolean
                                   }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { order: null
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.query.orderSummary(this.props.id))
      .then(order => this.setState({ order
                                   , initialised: true
                                   }))
      .catch(_ => this.setState({ initialised: true }))
  }

  delete = () => {
    this.props.request(ServerApi.command.deleteOrder(this.props.id)).then(_ => this.props.navigate('/orders'))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    const order = this.state.order
    if(!order) return <div>Order not found.</div>

    return (
      <div>
        <div><Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)}</h1>
        {!order.complete && !order.households.length ? <Link action={this.delete}>Delete</Link> : null}
        <Link action={_ => this.props.navigate('/orders/' + this.props.id + '/full')}>View full order</Link>
        <div>
          {order.households.map(h => (
            <div key={h.id}>
              <span>{h.name}</span>
              <Money amount={h.total} />
              <span>{h.cancelled && 'cancelled'}</span>
              {order.complete
                ? <Link action={_ => this.props.navigate('/orders/' + this.props.id + '/households/' + h.id)}>View</Link>
                : <Link action={_ => this.props.navigate('/orders/' + this.props.id + '/households/' + h.id)}>Manage</Link>
              }
            </div>
          ))}
          <div>
            <span>Total:</span>
            <Money amount={order.total} />
          </div>
        </div>
      </div>
    )
  }
}