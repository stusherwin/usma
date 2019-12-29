import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder, PastOrderItem, Household } from '../util/Types'
import { Util } from '../util/Util'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'
import { Collapsible, CollapsibleState } from '../util/Collapsible'
import { ServerApi } from '../util/ServerApi'

import { PastHouseholdOrderItem } from './PastHouseholdOrderItem'

export interface PastHouseholdOrdersProps { household: Household
                                          , collapsibleKey: string
                                          , collapsibleState: CollapsibleState
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export interface HouseholdPaymentsState { collapsibleState: CollapsibleState
                                        }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, HouseholdPaymentsState> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
    }
  }

  addToCurrentOrder = (item: PastOrderItem) => {
    if(!this.props.household.currentHouseholdOrder) return;

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.household.currentHouseholdOrder.orderId, this.props.household.id, item.productCode, null))
      .then(this.props.reload)
  }

  addAllItemsToCurrentOrder = (ho: PastHouseholdOrder) => {
    if(!this.props.household.currentHouseholdOrder) return;

    this.props.request(ServerApi.command.ensureAllItemsFromPastHouseholdOrder(this.props.household.currentHouseholdOrder.orderId, this.props.household.id, ho.orderId))
      .then(this.props.reload)
  }

  render() {
    const pastOrders = this.props.household.pastHouseholdOrders
    const total = pastOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)

    return (
      <Collapsible className="min-h-20"
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={
                     <div className="p-2 bg-past-order-lighter min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Past orders
                       </h2>
                       <h3 className="flex justify-end ml-20 mt-4">
                         {/* <span>Total:</span> */}
                         <span className="w-24 text-right"><Money amount={total} /></span>
                       </h3>
                     </div>
                   }>
        <div className="shadow-inner-top bg-order-lightest">
          { !pastOrders.length
          ? <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div>
          : <table className="border-collapse w-full">
              <tbody>
                { pastOrders.map((ho, i) => {
                  let status = 
                    <span>
                      { ho.isAbandoned?
                        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Abandoned</span>
                      : <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Complete</span>
                      }
                    </span>

                  return (
                    <tr key={ho.orderId}>
                      <td>
                        <Collapsible className="min-h-20"
                                     collapsibleKey={ho.orderId}
                                     collapsibleState={this.state.collapsibleState}
                                     header={
                                       <div className={classNames('p-2 bg-order-lightest min-h-20', {"shadow-inner-top": i == 0})}>
                                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                                         <h3 className="leading-none ml-20 relative flex">
                                           {Util.formatDate(ho.orderCreatedDate)}
                                         </h3>
                                         <h4 className="flex justify-between ml-20 mt-4 mb-4">
                                           {status}
                                           <span className="flex justify-end">
                                             {/* <span>Total:</span> */}
                                             <span className={classNames("w-24 font-bold text-right", {'line-through text-grey-darker': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                           </span>
                                         </h4>
                                         {this.props.household.currentHouseholdOrder && this.props.household.currentHouseholdOrder.isOpen && !!ho.items.length &&
                                           <div className="flex justify-end pt-2">
                                             <button onClick={e => {e.stopPropagation(); e.preventDefault(); this.addAllItemsToCurrentOrder(ho)}}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add all items to current order</button>
                                           </div>
                                         }
                                       </div>
                                     }>
                          <div className="shadow-inner-top bg-grey-lighter">
                            {!ho.items.length?
                              <div className="px-2 py-4 text-grey-darker">
                                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                              </div>
                            : <div>
                                <table className="border-collapse w-full">
                                  <tbody>
                                    {ho.items.map((item, index) => 
                                      <PastHouseholdOrderItem item={item} 
                                                              index={index} 
                                                              currentHouseholdOrder={this.props.household.currentHouseholdOrder}
                                                              addToCurrentOrder={this.addToCurrentOrder} />
                                    )}
                                    <tr>
                                      <td></td>
                                      <td className="pt-2 pr-2 pl-2" colSpan={3}>
                                        <div className="flex justify-between">
                                          <span>VAT:</span>
                                          <span className={classNames('text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat - ho.totalExcVat} /></span>
                                        </div>
                                      </td>
                                    </tr>
                                    <tr>
                                      <td></td>
                                      <td className="pt-4 pb-4 pl-2 pr-2 font-bold" colSpan={3}>
                                        <div className="flex justify-between">
                                          <span>Total:</span>
                                          <span className={classNames('text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                        </div>
                                      </td>
                                    </tr>
                                  </tbody>
                                </table>
                              </div>
                            }
                          </div>
                        </Collapsible>
                    </td>
                  </tr>
                )}) }
                <tr>
                  <td className="pt-4 pl-20 pr-2 pb-4 font-bold">
                    <div className="pl-2 flex justify-between">
                      <span>Total:</span>
                      <span className="w-24 font-bold text-right"><Money amount={total} /></span>
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>
          }
        </div>
      </Collapsible>
    )
  }
}