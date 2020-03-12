import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder } from 'util/Types'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'

import { OrderItem } from 'order/OrderItem'
import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'
import { OrderFooter } from 'order/OrderFooter'

export interface PastHouseholdOrdersProps { pastOrder: CollectiveOrder 
                                          }

export interface PastHouseholdOrdersState { collapsibleState: CollapsibleState }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, PastHouseholdOrdersState> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
    }
  }

  render() {
    return (
      <table className="border-collapse w-full">
        <tbody>
          {this.props.pastOrder.householdOrders.map((ho, i) => {
            return (
              <tr key={ho.householdId}>
                <td colSpan={2}>
                  <Collapsible collapsibleKey={ho.householdId}
                               collapsibleState={this.state.collapsibleState}
                               header={
                                 <div className={classNames('p-2 bg-household-light-sepia min-h-24')}>
                                   <div className="bg-no-repeat w-16 h-16 absolute bg-img-household sepia mt-2"></div>
                                   <div className="flex items-baseline justify-between mt-2 ml-20">
                                     <div>
                                       <h3 className={classNames("leading-none", {'line-through': ho.isAbandoned})}>
                                         {ho.householdName}
                                       </h3>
                                       <h4 className="flex justify-between mt-4 mb-4">
                                         <OrderStatus order={ho} />
                                       </h4>
                                     </div>
                                     <h4 className="ml-2">
                                       <OrderTotal order={ho} />
                                     </h4>
                                   </div>
                                 </div>
                               }>
                    <div className="shadow-inner-top bg-white-sepia">
                      {!ho.items.length?
                        <div className="px-2 py-4 text-black">
                          <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                        </div>
                      : <table className="border-collapse w-full shadow-inner-top">
                          <tbody>
                            {ho.items.map((item, index) =>
                              <OrderItem item={item} 
                                         past={true}
                                         orderAbandoned={ho.isAbandoned} />
                            )}
                            <OrderFooter order={ho} />
                          </tbody>
                        </table>
                      } 
                    </div>
                  </Collapsible>
                </td>
              </tr>
            )}
          )}
          <tr>
            <td className="pt-4 pb-4 pl-20 pr-2 font-bold" colSpan={2}>
              <div className="pl-2 flex justify-between">
                <span>Total:</span>
                <span className={classNames('text-right', {'line-through text-black': this.props.pastOrder.isAbandoned})}><Money amount={this.props.pastOrder.totalIncVat} /></span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    )
  }  
}