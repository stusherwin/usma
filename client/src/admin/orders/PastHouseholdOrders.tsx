import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder } from '../../util/Types'
import { Icon } from '../../util/Icon'
import { Money } from '../../util/Money'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { OrderItem } from '../../household/OrderItem'

export interface PastHouseholdOrdersProps { pastOrder: PastCollectiveOrder 
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
      <table className="border-collapse w-full bg-grey-lighter shadow-inner-top">
        <tbody>
          {this.props.pastOrder.pastHouseholdOrders.map((ho, i) => {
            let status = 
              <span>
                {ho.isAbandoned?
                  <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Abandoned</span>
                : <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Complete</span>
                }
              </span>

            return (
              <tr key={ho.householdId}>
                <td colSpan={2}>
                  <Collapsible className="min-h-24"
                               collapsibleKey={ho.householdId}
                               collapsibleState={this.state.collapsibleState}
                               header={
                                 <div className={classNames('p-2 bg-household-lighter min-h-24', {'shadow-inner-top': i == 0})}>
                                   <div className="bg-no-repeat w-16 h-16 absolute bg-img-household mt-2"></div>
                                   <h3 className="leading-none ml-20 relative flex mt-2">
                                     {ho.householdName}
                                   </h3>
                                   <h4 className="flex justify-between ml-20 mt-4 mb-4">
                                     {status}
                                     <span className="flex justify-end">
                                       {/* <span>Total:</span> */}
                                       <span className={classNames("w-24 font-bold text-right", {'line-through text-grey-darker': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                     </span>
                                   </h4>
                                 </div>
                               }>
                    <div className="shadow-inner-top bg-white">
                      {!ho.items.length?
                        <div className="px-2 py-4 text-grey-darker">
                          <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                        </div>
                      : <table className="border-collapse w-full bg-grey-lighter shadow-inner-top">
                          <tbody>
                            {ho.items.map((item, index) =>
                              <OrderItem item={item} 
                                                         index={index} 
                                                         orderAbandoned={ho.isAbandoned} />
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
                <span className={classNames('text-right', {'line-through text-grey-dark': this.props.pastOrder.isAbandoned})}><Money amount={this.props.pastOrder.totalIncVat} /></span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    )
  }  
}