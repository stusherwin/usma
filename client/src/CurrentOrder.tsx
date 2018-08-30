import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'

export interface CurrentOrderProps { order: CollectiveOrder
                                   , householdOrders: HouseholdOrder[]
                                   , households: Household[]
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { addingHousehold: Household | null
                                }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  constructor(props: CurrentOrderProps) {
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

    this.props.request(ServerApi.command.createHouseholdOrder(this.props.order.id, this.state.addingHousehold.id))
      .then(this.props.reload)
      .then(_ => this.setState({ addingHousehold: null
                               }))
  }

  deleteHouseholdOrder = (h: HouseholdOrder) => {
    this.props.request(ServerApi.command.deleteHouseholdOrder(h.orderId, h.householdId))
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
    const allComplete = this.props.householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const householdsInOrder = this.props.households.filter(h => !!this.props.householdOrders.find(oh => oh.householdId == h.id))
    const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)

    return (
      <div>
        <div>
          <div>Status: {order.status}</div>
          {/* {!!this.props.order.items.length &&
            <RouterLink path={`/orders/${this.props.order.id}/full`}>View full order</RouterLink>
          } */}
          {order.canBeAmended && !this.props.householdOrders.length &&
            <Button disabled={!!this.state.addingHousehold} action={() => this.deleteOrder()}>Delete order</Button>
          }
          {order.canBeAmended && !!this.props.householdOrders.length && (
            !allComplete
            ? <div>Waiting for everyone to complete their orders...</div>
            : !allPaid
            ? <div>Waiting for everyone to pay up...</div>
            : <Button disabled={!!this.state.addingHousehold} action={() => this.placeOrder()}>Place order</Button>
          )}
        </div>
        <div>
          {!this.props.householdOrders.length &&
            'Waiting for households to join...'
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
                <RouterLink path={`/households/${ho.householdId}`}>{ho.householdName}</RouterLink>
                {status}
                <Money amount={ho.total} />
                {order.canBeAmended && !ho.items.length &&
                  <span>
                    <Button disabled={!!this.state.addingHousehold} action={() => this.deleteHouseholdOrder(ho)}>Delete</Button>
                  </span>
                }
              </div>
            )}
          )}
          {order.canBeAmended && !!unusedHouseholds.length && !this.state.addingHousehold &&
            <div>
              <Button action={() => this.startAddHousehold(unusedHouseholds[0])}>Add household</Button>
            </div>
          }
          {this.state.addingHousehold &&
            <div>
              <span>
                <select value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
                  {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
                </select>
              </span>
              <Button action={this.confirmAddHousehold}>Save</Button>
              <Button action={this.cancelAddHousehold}>Cancel</Button>
            </div>
          }
          <div>
            <span>Total:</span>
            <Money amount={order.total} />
          </div>
        </div>
      </div>
    )
  }
}