import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder } from '../../util/Types'
import { Util } from '../../util/Util'
import { Icon } from '../../util/Icon'
import { Money } from '../../util/Money'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { PastHouseholdOrders } from './PastHouseholdOrders';
import { CollectiveOrderTabs } from './CollectiveOrderTabs'

export interface PastCollectiveOrdersProps { pastOrders: PastCollectiveOrder[]
                                             collapsibleKey: string
                                             collapsibleState: CollapsibleState
                                           }
                                 
export interface PastCollectiveOrdersState { collapsibleState: CollapsibleState
                                             tab: 'households' | 'product-list' | 'product-codes'
                                           }

export class PastCollectiveOrders extends React.Component<PastCollectiveOrdersProps, PastCollectiveOrdersState> {  
  constructor(props: PastCollectiveOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState})),
      tab: 'households'
    }
  }

  render() {
    const pastOrders = this.props.pastOrders

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
                     </div>
                   }>
        <div className="bg-white shadow-inner-top">
          {!pastOrders.length
          ? <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div> 
          : (
            <div>
              { pastOrders.map((o, i) => 
                <Collapsible className="min-h-20"
                   collapsibleKey={o.id}
                   collapsibleState={this.state.collapsibleState}
                   header={
                     <div className={classNames('p-2 bg-order-lightest min-h-20', {"shadow-inner-top": i == 0})}>
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h3 className="leading-none ml-20 relative flex">
                         {Util.formatDate(o.createdDate)}
                       </h3>
                       <h4 className="flex justify-between ml-20 mt-4 mb-4">
                         <span>
                           { o.isAbandoned?
                             <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Abandoned</span>
                           : <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Complete</span>
                           }
                         </span>
                         <span className="flex justify-end">
                           <span className={classNames("w-24 font-bold text-right", {'line-through text-grey-darker': o.isAbandoned})}><Money amount={o.totalIncVat} /></span>
                         </span>
                       </h4>
                       <CollectiveOrderTabs tab={this.state.tab} setTab={tab => this.setState({tab})} />
                     </div>
                   }>
                    <div>
                      <div>
                        <PastHouseholdOrders pastOrder={o} />
                      </div>
                    </div>
                </Collapsible>
              )}
            </div>
          )}
        </div>
      </Collapsible>
    )
  } 
}