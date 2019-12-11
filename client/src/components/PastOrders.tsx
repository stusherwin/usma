import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder, PastHouseholdOrder, PastOrderItem } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible } from '../common/Collapsible'
import { PastHouseholdOrders } from './PastHouseholdOrdersForOrder';

export interface PastOrdersProps { pastOrders: PastCollectiveOrder[]
                                 , pastHouseholdOrders: PastHouseholdOrder[]
                                 , expanded: boolean
                                 , otherExpanding: boolean
                                 , toggle: () => void
                                 }
export interface PastOrdersState { expanded: number | null }

export class PastOrders extends React.Component<PastOrdersProps, PastOrdersState> {  
  constructor(props: PastOrdersProps) {
    super(props)

    this.state = { 
      expanded: null, 
    }
  }

  toggle = (toExpand: PastCollectiveOrder) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand.id == expanded? null : toExpand.id}));
  }

  render() {
    const pastOrders = this.props.pastOrders

    return (
      <Collapsible className="min-h-20"
                   {...this.props}
                   header={() => 
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
                   expanded={this.state.expanded == o.id}
                   otherExpanding={!!this.state.expanded && this.state.expanded != o.id}
                   toggle={this.toggle(o)}
                   header={() => 
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
                     </div>
                   }>
                    <div>
                      <div>
                        <PastHouseholdOrders pastOrder={o}
                                             pastHouseholdOrders={this.props.pastHouseholdOrders.filter(ho => ho.orderId == o.id)} />
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