import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Icon } from '../../util/Icon'
import { Money } from '../../util/Money'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { CurrentHouseholdOrder } from '../../household/CurrentHouseholdOrder'

export interface HouseholdOrdersProps { order: CollectiveOrder
                                      , householdOrders: HouseholdOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

export interface HouseholdOrdersState { collapsibleState: CollapsibleState }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, HouseholdOrdersState> {
  constructor(props: HouseholdOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
    }
  }

  render() {
    const order = this.props.order

    return (
        !this.props.householdOrders.length?
          <div className="px-2 py-4 text-grey-darker">
            <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order yet
          </div>
        : <div> 
            {!!order.items.length &&
              <div className="flex justify-end mt-4 mr-2">
                <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/household-orders-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
              </div>
            }
            <div className="mt-4">
                {this.props.householdOrders.map((ho, i) => {
                  let status = 
                    <span>
                      { ho.isComplete?
                        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Complete</span>
                      : ho.isAbandoned?
                        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Abandoned</span>
                      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Open</span>
                      }
                    </span>

                  return (
                    <div key={ho.householdId}>
                      <Collapsible className="min-h-16"
                                   collapsibleKey={ho.householdId}
                                   collapsibleState={this.state.collapsibleState}
                                   header={
                                     <div className={classNames('p-2 bg-household-lighter min-h-16')}>
                                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                                       <h3 className="leading-none ml-20 relative flex">
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
                        <div className="shadow-inner-top bg-white border-t border-household-light">
                          {!ho.items.length?
                            <div className="px-2 py-4 text-grey-darker">
                              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet
                            </div>
                          : <CurrentHouseholdOrder currentHouseholdOrder={ho}
                                                   readOnly={true}
                                                   {...this.props} />
                          }
                        </div>
                      </Collapsible>
                    </div>
                  )}
                )}
                <div className="pt-4 pb-4 pl-20 pr-2 font-bold text-black pl-2 flex justify-between">
                  <span>Total:</span>
                  <span className="w-24 font-bold text-right"><Money amount={order.totalIncVat} /></span>
                </div>
            </div>
          </div>
    )
  }
}