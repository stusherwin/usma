import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Icon } from './Icon'
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
    const orderMinimumReached = this.props.order.total >= 25000

    const canDeleteOrder = !this.props.householdOrders.length
    const canAddHousehold = !!unusedHouseholds.length
    const canPlaceOrder = !!this.props.householdOrders.length && allComplete && allPaid && orderMinimumReached

    const deletableHousehold = !!this.props.householdOrders.length && !!this.props.householdOrders.find(ho => !ho.items.length)

    return (
      <div>
        <div className="bg-order-dark p-2 pt-0">
          <h3 className="mt-0">{order.status}</h3>
          <div className="mt-2">
            {order.canBeAmended && (
              !this.props.householdOrders.length
              ? <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for households to join</span>
              : !orderMinimumReached
              ? <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for &pound;250.00 order minimum</span>
              : !allComplete
              ? <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for all orders to be completed</span>
              : !allPaid
              ? <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
              : <span className="text-green"><Icon type="ok" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Good to go</span>
            )}
          </div>
          {order.canBeAmended && (canDeleteOrder || canAddHousehold || canPlaceOrder) &&
            <div className="mt-2">
              {/* {!!this.props.order.items.length &&
                <RouterLink path={`/orders/${this.props.order.id}/full`}>View full order</RouterLink>
              } */}
              {canDeleteOrder &&
                <Button className="mr-2" disabled={!!this.state.addingHousehold} action={() => this.deleteOrder()}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</Button>
              }
              {canAddHousehold &&
                <Button className="mr-2" disabled={!!this.state.addingHousehold} action={() => this.startAddHousehold(unusedHouseholds[0])}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add household</Button>
              }
              {canPlaceOrder &&
                <Button disabled={!!this.state.addingHousehold} action={() => this.placeOrder()}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</Button>
              }
            </div>
          }
        </div>
        {this.state.addingHousehold &&
          <div className="bg-household-lightest p-2">
            <h3 className="mb-4">Add household</h3>
            <select className="mb-4 w-full" value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
              {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
            </select>
            <div className="flex justify-end">
              <Button className="ml-2" action={this.confirmAddHousehold}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</Button>
              <Button className="ml-2" action={this.cancelAddHousehold}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</Button>
            </div>
          </div>
        }
        <table className="border-collapse w-full">
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
              <tr key={ho.householdId}>
                <td className="pt-2 pl-2 pr-2"><RouterLink path={`/households/${ho.householdId}`}>{ho.householdName}</RouterLink></td>
                <td className="pt-2 pr-2">{status}</td>
                <td className="pt-2 pr-2 text-right"><Money amount={ho.total} /></td>
                {deletableHousehold && 
                  <td className="pt-2 pr-2 w-1">
                    {order.canBeAmended && !ho.items.length &&
                      <Button disabled={!!this.state.addingHousehold} action={() => this.deleteHouseholdOrder(ho)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></Button>
                    }
                  </td>
                }
              </tr>
            )}
          )}
          {!!this.props.householdOrders.length &&
            <tr>
              <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
              <td className="pt-2 pr-2"></td>
              <td className="pt-2 pr-2 font-bold text-right"><Money amount={order.total} /></td>
              {deletableHousehold && 
                <td className="pt-2 pr-2 w-1"><div className="bg-transparent w-9 h-6"></div></td>
              }
            </tr>
          }
        </table>
      </div>
    )
  }
}