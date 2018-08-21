import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrderPageProps { order: CollectiveOrder
                                , households: Household[]
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                , reload: () => void
                                }

export interface OrderPageState { addingHousehold: Household | null
                                }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { addingHousehold: null
                 }
  }

  cancel = () => {
    this.props.request(ServerApi.command.cancelOrder(this.props.order.id))
      .then(this.props.reload)
  }

  uncancel = () => {
    this.props.request(ServerApi.command.uncancelOrder(this.props.order.id))
      .then(this.props.reload)
  }

  startAddHousehold = (h: Household) => this.setState({ addingHousehold: h })

  addingHouseholdChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingHousehold: this.props.households.find(h => '' + h.id == event.target.value) || null })

  cancelAddHousehold = () =>
    this.setState({ addingHousehold: null
                  })

  confirmAddHousehold = () => {
    if(!this.state.addingHousehold) return

    this.props.request(ServerApi.command.addHouseholdOrder(this.props.order.id, this.state.addingHousehold.id))
      .then(_ => {
        this.setState({ addingHousehold: null
                      })
        this.props.reload()
      })
  }

  removeHousehold = (h: HouseholdOrder) => {
    this.props.request(ServerApi.command.removeHouseholdOrder(h.orderId, h.householdId))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const unusedHouseholds = this.props.households.filter(h => !order.householdOrders.find(oh => oh.householdId == h.id))

    return (
      <div>
        <div><Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)} {order.cancelled && ' (cancelled)'}</h1>
        {!order.complete && (
          order.cancelled
          ? <Link action={this.uncancel}>Uncancel order</Link>
          : <Link action={this.cancel}>Cancel order</Link>
        )}
        <Link action={_ => this.props.navigate('/orders/' + this.props.order.id + '/full')}>View full order</Link>
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
          {order.householdOrders.map(h => (
            <div key={h.householdId}>
              <span>{h.householdName}</span>
              <Money amount={h.total} />
              <span>{h.cancelled && 'cancelled'}</span>
              {order.complete
                ? <Link action={_ => this.props.navigate('/orders/' + h.orderId + '/households/' + h.householdId)}>View</Link>
                : <Link action={_ => this.props.navigate('/orders/' + h.orderId + '/households/' + h.householdId)}>Manage</Link>
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