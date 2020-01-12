import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder } from '../../util/Types'
import { Util } from '../../util/Util'
import { Icon } from '../../util/Icon'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'
import { ServerApi } from '../../util/ServerApi'

import { PastHouseholdOrders } from './PastHouseholdOrders';
import { OrderTabs, OrderTab } from '../../order/OrderTabs'
import { OrderItems } from '../../order/OrderItems'
import { ProductCodes } from './ProductCodes'
import { OrderStatus } from '../../order/OrderStatus'
import { OrderTotal } from '../../order/OrderTotal'

export interface PastCollectiveOrdersProps { pastOrders: PastCollectiveOrder[]
                                             collapsibleKey: string
                                             collapsibleState: CollapsibleState
                                           }
                                 
export interface PastCollectiveOrdersState { collapsibleState: CollapsibleState
                                             tabs: OrderTab[]
                                           }

export class PastCollectiveOrders extends React.Component<PastCollectiveOrdersProps, PastCollectiveOrdersState> {  
  constructor(props: PastCollectiveOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState})),
      tabs: []
    }
  }

  setTab = (i: number) => (tab: OrderTab) => {
    let tabs = this.state.tabs
    tabs[i] = tab
    this.setState({tabs})
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
                     <div className={classNames('p-2 bg-past-order-lightest min-h-20', {"shadow-inner-top": i == 0})}>
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h3 className="leading-none ml-20 relative flex">
                         {Util.formatDate(o.createdDate)}
                       </h3>
                       <h4 className="flex justify-between ml-20 mt-4 mb-4">
                         <OrderStatus order={o} />
                         <OrderTotal order={o} />
                       </h4>
                       <div className="mt-5">
                         <OrderTabs tab={this.state.tabs[i]} setTab={this.setTab(i)} />
                       </div>
                     </div>
                   }>
                  { (this.state.tabs[i] || 'households') == 'households'?
                    <div className="shadow-inner-top border-t bg-household-lightest">
                      <div className="flex justify-end mt-4 mr-2 mb-4">
                        <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url(`query/past-household-orders-download/${o.id}`)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                      </div>
                      <PastHouseholdOrders pastOrder={o} />
                    </div>
                  : this.state.tabs[i] == 'product-list'?
                    <div className="shadow-inner-top border-t bg-white">
                      <div className="flex justify-end mr-2 mt-4 mb-4">
                        <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url(`query/past-collective-order-download/${o.id}`)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                      </div>
                      <OrderItems order={o} />
                    </div>
                  : <div className="shadow-inner-top border-t bg-white">
                      <ProductCodes order={o} />
                    </div>
                  }
                </Collapsible>
              )}
            </div>
          )}
        </div>
      </Collapsible>
    )
  } 
}