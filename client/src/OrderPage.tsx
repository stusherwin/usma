import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'
import { Router } from './Router'

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

  deleteOrder = () => {
    this.props.request(ServerApi.command.deleteOrder(this.props.order.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/orders`))
  }

  placeOrder = () => {
    this.props.request(ServerApi.command.placeOrder(this.props.order.id))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const unusedHouseholds = this.props.households.filter(h => !this.props.householdOrders.find(oh => oh.householdId == h.id))
    const householdsInOrder = this.props.households.filter(h => !!this.props.householdOrders.find(oh => oh.householdId == h.id))
    const allPaid = householdsInOrder.reduce((p, h) => p && h.balance > 0, true)

    return (
      <div>
        <div><RouterLink path="/orders">Orders</RouterLink> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)} ({order.status})</h1>
          <div>Status: {order.status}</div>
          <div>
          {!!this.props.order.items.length &&
            <RouterLink path={`/orders/${this.props.order.id}/full`}>View full order</RouterLink>
          }
          {order.canBeAmended && !this.props.householdOrders.length &&
            <Link disabled={!!this.state.addingHousehold} action={() => this.deleteOrder()}>Delete order</Link>
          }
          {order.canBeAmended && (
            allPaid
            ? (
                <Link disabled={!!this.state.addingHousehold} action={() => this.placeOrder()}>Place order</Link>
            )
            : 'Waiting for all households to pay...'
          )}
        </div>
        <h2>Households</h2>
        <div>
          {!this.props.householdOrders.length &&
            'Waiting for households to join...'
          }
          {order.canBeAmended && !!unusedHouseholds.length && !this.state.addingHousehold &&
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
          {this.props.householdOrders.map(ho => {
            let household = this.props.households.find(h => h.id == ho.householdId)
            let status = (
              <span>
                {ho.status}
                {ho.isComplete &&
                  <span>
                    {household && (
                      household.balance > 0
                      ? '(paid)'
                      : (<span>(<Money amount={household.balance} absolute /> to pay <RouterLink path={`/households/${ho.householdId}`}>Make payment</RouterLink>)</span>)
                    )}
                  </span>
                }
              </span>
            )
            return (
              <div key={ho.householdId}>
                <span>{ho.householdName}</span>
                <Money amount={ho.total} />
                {status}
                <RouterLink path={`/orders/${ho.orderId}/households/${ho.householdId}`}>View</RouterLink>
                {order.canBeAmended && !ho.items.length &&
                  <span>
                    <Link disabled={!!this.state.addingHousehold} action={() => this.removeHousehold(ho)}>Remove</Link>
                  </span>
                }
              </div>
            )}
          )}
          <div>
            <span>Total:</span>
            <Money amount={order.total} />
          </div>
        </div>
      </div>
    )
  }
}