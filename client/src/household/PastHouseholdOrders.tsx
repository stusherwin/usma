import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem as Item, Household } from 'util/Types'
import { Util } from 'util/Util'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { OrderItem } from 'order/OrderItem'
import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'
import { OrderFooter } from 'order/OrderFooter'

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

  addToCurrentOrder = (item: Item) => {
    if(!this.props.household.currentHouseholdOrder) return;

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.household.currentHouseholdOrder.orderId, this.props.household.id, item.productCode, null))
      .then(this.props.reload)
  }

  addAllItemsToCurrentOrder = (ho: HouseholdOrder) => {
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
                         <span className="text-right"><Money amount={total} /></span>
                       </h3>
                     </div>
                   }>
        <div className="shadow-inner-top bg-past-order-lightest">
          { !pastOrders.length
          ? <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div>
          : <table className="border-collapse w-full">
              <tbody>
                { pastOrders.map((ho, i) => {
                  return (
                    <tr key={ho.orderId}>
                      <td>
                        <Collapsible className="min-h-20"
                                     collapsibleKey={ho.orderId}
                                     collapsibleState={this.state.collapsibleState}
                                     header={
                                       <div className={classNames('p-2 bg-past-order-lightest min-h-20', {"shadow-inner-top": i == 0})}>
                                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                                         <h3 className="leading-none ml-20 relative flex">
                                           {Util.formatDate(ho.orderCreatedDate)}
                                         </h3>
                                         <h4 className="flex justify-between ml-20 mt-4 mb-4">
                                           <OrderStatus order={ho} />
                                           <OrderTotal order={ho} />
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
                                      <OrderItem item={item} 
                                                 index={index} 
                                                 orderAbandoned={ho.isAbandoned}
                                                 addToCurrentOrder={!!this.props.household.currentHouseholdOrder && this.props.household.currentHouseholdOrder.isOpen && this.addToCurrentOrder || undefined} />
                                    )}
                                    <OrderFooter order={ho} />
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
                      <span className="font-bold text-right"><Money amount={total} /></span>
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