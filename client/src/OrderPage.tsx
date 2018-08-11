import * as React from 'react';

import { OrderSummary, Household, OrderSummary_Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrderPageProps { id: number
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export interface OrderPageState { order: OrderSummary | null
                                , households: Household[]
                                , initialised: boolean
                                , addingHousehold: Household | null
                                }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { order: null
                 , households: []
                 , initialised: false
                 , addingHousehold: null
                 }
  }

  componentDidMount() {
    this.props.request(Promise.all([ServerApi.query.orderSummary(this.props.id), ServerApi.query.households()]))
      .then(results => this.setState({ order: results[0]
                                     , households: results[1]
                                     , initialised: true
                                     }))
      .catch(_ => this.setState({ initialised: true }))
  }

  delete = () => {
    this.props.request(ServerApi.command.deleteOrder(this.props.id)).then(_ => this.props.navigate('/orders'))
  }

  startAddHousehold = (h: Household) => this.setState({ addingHousehold: h })

  addingHouseholdChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingHousehold: this.state.households.find(h => '' + h.id == event.target.value) || null })

  cancelAddHousehold = () =>
    this.setState({ addingHousehold: null
                  })

  confirmAddHousehold = () => {
    if(!this.state.addingHousehold) return

    this.props.request(ServerApi.command.addHouseholdOrder(this.props.id, this.state.addingHousehold.id))
      .then(() => this.props.request(Promise.all([ServerApi.query.orderSummary(this.props.id), ServerApi.query.households()])))
      .then(results => this.setState({ order: results[0]
                                     , households: results[1]
                                     , addingHousehold: null
                                     }))
  }

  removeHousehold = (h: OrderSummary_Household) => {
    this.props.request(ServerApi.command.removeHouseholdOrder(this.props.id, h.id))
      .then(() => this.props.request(Promise.all([ServerApi.query.orderSummary(this.props.id), ServerApi.query.households()])))
      .then(results => this.setState({ order: results[0]
                                     , households: results[1]
                                     }))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    const order = this.state.order
    if(!order) return <div>Order not found.</div>
    let unusedHouseholds = this.state.households.filter(h => !order.households.find(oh => oh.id == h.id))

    return (
      <div>
        <div><Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)}</h1>
        {!order.complete && !order.households.length ? <Link action={this.delete}>Delete</Link> : null}
        <Link action={_ => this.props.navigate('/orders/' + this.props.id + '/full')}>View full order</Link>
        {!order.complete && !!unusedHouseholds.length &&
          <Link action={() => this.startAddHousehold(unusedHouseholds[0])}>Add household</Link>
        }
        {this.state.addingHousehold &&
          <div>
            <span>
              <select value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
                {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
              </select>
            </span>
            <Link action={this.confirmAddHousehold}>Add</Link>
            <Link action={this.cancelAddHousehold}>Cancel</Link>
          </div>
        }
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
              {!order.complete && !h.total &&
                <span>
                  <Link disabled={!!this.state.addingHousehold} action={() => this.removeHousehold(h)}>Remove</Link>
                </span>
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