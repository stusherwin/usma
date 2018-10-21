import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
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
    Router.navigate('/orders/place')
  }

  cancelOrder = () => {
    this.props.request(ServerApi.command.cancelOrder(this.props.order.id))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const unusedHouseholds = this.props.households.filter(h => !this.props.householdOrders.find(oh => oh.householdId == h.id))
    const allComplete = this.props.householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const householdsInOrder = this.props.households.filter(h => !!this.props.householdOrders.find(oh => oh.householdId == h.id))
    const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)
    const orderMinimumReached = this.props.order.total >= 25000

    const deleteOrderPossible = !this.props.householdOrders.length
    const addHouseholdPossible = !!unusedHouseholds.length
    const placeOrderPossible = !!this.props.householdOrders.length
    const placeOrderAllowed = allComplete && allPaid && orderMinimumReached
    const cancelOrderPossible = !!this.props.householdOrders.length

    const deletableHousehold = !!this.props.householdOrders.length && !!this.props.householdOrders.find(ho => !ho.items.length)

    return (
      <div>
        <div className="bg-order-dark p-2 pt-0">
          <div className="">
            {!this.props.householdOrders.length?
                <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for households to join</span>
              : !orderMinimumReached?
                <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for &pound;250.00 order minimum</span>
              : !allComplete?
                <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for all orders to be completed</span>
              : !allPaid?
                <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
              : <span className="text-green"><Icon type="ok" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Good to go</span>
            }
          </div>
          {(deleteOrderPossible || placeOrderPossible || cancelOrderPossible) &&
            <div className="mt-2">
              {deleteOrderPossible &&
                <button className="mr-2 mt-2" disabled={!!this.state.addingHousehold} onClick={this.deleteOrder}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
              }
              {cancelOrderPossible &&
                <button className="mr-2 mt-2" disabled={!!this.state.addingHousehold} onClick={this.cancelOrder}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Cancel order</button>
              }
              {placeOrderPossible &&
                <button className="mr-2 mt-2" disabled={!!this.state.addingHousehold || !placeOrderAllowed} onClick={this.placeOrder}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
              }
            </div>
          }
        </div>
        {!this.state.addingHousehold && addHouseholdPossible &&
          <div className="p-2 flex justify-end">
            <button disabled={!!this.state.addingHousehold} onClick={_ => this.startAddHousehold(unusedHouseholds[0])}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add a household</button>
          </div>
        }
        {!this.state.addingHousehold && !this.props.householdOrders.length &&
          <div className="text-grey-darker p-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order yet</div>
        }
        {this.state.addingHousehold &&
          <div className="bg-household-lightest p-2">
            <h3 className="mb-4">Add household</h3>
            <select className="mb-4 w-full" value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
              {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
            </select>
            <div className="flex justify-end">
              <button className="ml-2" onClick={this.confirmAddHousehold}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button>
              <button className="ml-2" onClick={this.cancelAddHousehold}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
            </div>
          </div>
        }
        {!!this.props.householdOrders.length &&
          <table className="border-collapse w-full mb-4">
            <tbody>
              {this.props.householdOrders.map(ho => {
                let household = this.props.households.find(h => h.id == ho.householdId)
                let status = (
                  <span>
                    {ho.status}
                    {ho.isComplete &&
                      <span>
                        {household && (
                          household.balance > 0
                          ? ' (paid)'
                          : (<span>(<Money amount={household.balance} absolute /> to pay <RouterLink path={`/households/${ho.householdId}`}>Make payment</RouterLink>)</span>)
                        )}
                      </span>
                    }
                  </span>
                )
                return (
                  <tr key={ho.householdId} className={classNames({'crossed-out': ho.status == 'Cancelled'})}>
                    <td className="pt-2 pl-2 pr-2"><RouterLink path={`/households/${ho.householdId}`}>{ho.householdName}</RouterLink></td>
                    <td className="pt-2 pr-2">{status}</td>
                    <td className="pt-2 pr-2 text-right"><Money amount={ho.total} /></td>
                    {deletableHousehold && 
                      <td className="pt-2 pr-2 w-1">
                        {!ho.items.length &&
                          <button disabled={!!this.state.addingHousehold} onClick={_ => this.deleteHouseholdOrder(ho)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                        }
                      </td>
                    }
                  </tr>
                )}
              )}
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={order.total} /></td>
                {deletableHousehold && 
                  <td className="pt-2 pr-2 w-1"><div className="bg-transparent w-9 h-6"></div></td>
                }
              </tr>
            </tbody>
          </table>
        }
      </div>
    )
  }
}