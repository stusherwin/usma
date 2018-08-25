import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'

export interface OrderPageProps { order: CollectiveOrder
                                , householdOrders: HouseholdOrder[]
                                , households: Household[]
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , reload: () => Promise<void>
                                }

export interface OrderPageState { addingHousehold: Household | null
                                }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { addingHousehold: null
                 }
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
      .then(this.props.reload)
      .then(_ => this.setState({ addingHousehold: null
                               }))
  }

  removeHousehold = (h: HouseholdOrder) => {
    this.props.request(ServerApi.command.removeHouseholdOrder(h.orderId, h.householdId))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const unusedHouseholds = this.props.households.filter(h => !this.props.householdOrders.find(oh => oh.householdId == h.id))

    return (
      <div>
        <div><RouterLink path="/orders">Orders</RouterLink> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)} {order.isCancelled && ' (cancelled)'}</h1>
        {!!this.props.order.items.length &&
          <RouterLink path={`/orders/${this.props.order.id}/full`}>View full order</RouterLink>
        }
        <h2>Households</h2>
        <div>
          {!this.props.householdOrders.length &&
            <div>No households added to this order</div>
          }
          {!order.isComplete && !order.isCancelled && !!unusedHouseholds.length && !this.state.addingHousehold &&
            <div>
              <Link action={() => this.startAddHousehold(unusedHouseholds[0])}>Add household</Link>
            </div>
          }
          {this.state.addingHousehold &&
            <div>
              <span>
                <select value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
                  {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
                </select>
              </span>
              <Link action={this.confirmAddHousehold}>Save</Link>
              <Link action={this.cancelAddHousehold}>Cancel</Link>
            </div>
          }
          {this.props.householdOrders.map(h => (
            <div key={h.householdId}>
              <span>{h.householdName}</span>
              <Money amount={h.total} />
              <span>{h.isCancelled && 'cancelled'}</span>
              <RouterLink path={`/orders/${h.orderId}/households/${h.householdId}`}>View</RouterLink>
              {!order.isComplete && !order.isCancelled && !h.total &&
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