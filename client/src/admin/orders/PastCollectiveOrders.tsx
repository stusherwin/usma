import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder } from 'util/Types'
import { Util } from 'util/Util'
import { Icon } from 'util/Icon'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { OrderTabs, OrderTab } from 'order/OrderTabs'
import { OrderItems } from 'order/OrderItems'
import { ProductCodes } from './ProductCodes'
import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'

import { PastHouseholdOrders } from './PastHouseholdOrders';
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { ReconcileOrder } from './ReconcileOrder'

export interface PastCollectiveOrdersProps { pastOrders: CollectiveOrder[]
                                             collapsibleKey: string
                                             collapsibleState: CollapsibleState
                                             request: <T extends {}>(p: Promise<T>) => Promise<T>
                                             reload: () => Promise<void>
                                           }
                                 
export interface PastCollectiveOrdersState { collapsibleState: CollapsibleState
                                             tabs: OrderTab[]
                                             reconcilingOrder: {[orderId: number]: boolean}
                                           }

export class PastCollectiveOrders extends React.Component<PastCollectiveOrdersProps, PastCollectiveOrdersState> {  
  constructor(props: PastCollectiveOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState})),
      tabs: [],
      reconcilingOrder: {},
    }
  }

  setTab = (i: number) => (tab: OrderTab) => {
    let tabs = this.state.tabs
    tabs[i] = tab
    this.setState({tabs})
  }

  startReconcilingOrder = (order: CollectiveOrder) => () => {
    this.state.reconcilingOrder[order.id] = true
    this.setState({reconcilingOrder: {...this.state.reconcilingOrder}})
  }

  endReconcilingOrder = (order: CollectiveOrder) => () => {
    this.state.reconcilingOrder[order.id] = false
    this.setState({reconcilingOrder: {...this.state.reconcilingOrder}})
  }

  endReconcilingItem = (order: CollectiveOrder) => (productId: number, productPriceExcVat: number, households: {householdId: number, itemQuantity: number}[]) => {
    this.props.request(ServerApi.command.reconcileOrderItem(order.id, productId, productPriceExcVat, households))
      .then(this.props.reload)
  }

  render() {
    const pastOrders = this.props.pastOrders

    return (
      <Collapsible collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={ref =>
                     <div ref={ref} className="p-2 pt-4 bg-past-orders-sepia border-past-orders-sepia-dark border-b border-t min-h-24">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order sepia"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Past orders
                       </h2>
                     </div>
                   }>
        <div className="bg-white shadow-inner-top">
          {!pastOrders.length
          ? <div className="px-2 py-4 text-black">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div> 
          : (
            <div>
              { pastOrders.map((o, i) => 
                <Collapsible
                   collapsibleKey={o.id}
                   collapsibleState={this.state.collapsibleState}
                   onCollapsed={this.endReconcilingOrder(o)}
                   header={ref =>
                     <div ref={ref} className={classNames('p-2 pt-4 bg-order-dark-sepia min-h-24', {"shadow-inner-top": i == 0})}>
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order sepia"></div>
                       <div className="flex items-baseline justify-between ml-20">
                         <div>
                           <h3 className={classNames("leading-none", {'line-through': o.isAbandoned})}>
                             {Util.formatDate(o.orderCreatedDate)}
                           </h3>
                           <h4 className="mt-4 mb-4">
                             <OrderStatus order={o} />
                           </h4>
                         </div>
                         <h4 className="ml-2">
                           <OrderTotal order={o} />
                         </h4>
                       </div>
                     </div>
                   }
                   expandedHeader={!this.state.reconcilingOrder[o.id] && 
                     <div className="p-2 -mt-4 bg-order-dark-sepia">
                       <CollectiveOrderButtons order={o}
                                               reconcileOrder={this.startReconcilingOrder(o)} />
                       <div className="mt-5">
                         <OrderTabs tab={this.state.tabs[i]} 
                                    setTab={this.setTab(i)} 
                                    householdsBg="bg-household-lightest-sepia"
                                    productsBg="bg-white-sepia"
                                    productCodesBg="bg-white-sepia" />
                       </div>
                     </div>
                   || undefined}>
                  { this.state.reconcilingOrder[o.id]?
                    <ReconcileOrder order={o} 
                                    past={true}
                                    endReconcilingOrder={this.endReconcilingOrder(o)}
                                    endReconcilingItem={this.endReconcilingItem(o)} />
                  : (this.state.tabs[i] || 'households') == 'households'?
                    <div className="shadow-inner-top border-t bg-household-lightest-sepia">
                      <div className="flex justify-end mt-4 mr-2 mb-4">
                        <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url(`query/past-household-orders-download/${o.id}`)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                      </div>
                      <PastHouseholdOrders pastOrder={o} />
                    </div>
                  : this.state.tabs[i] == 'product-list'?
                    <div className="shadow-inner-top border-t bg-white-sepia">
                      <div className="flex justify-end mr-2 mt-4 mb-4">
                        <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url(`query/past-collective-order-download/${o.id}`)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                      </div>
                      <OrderItems order={o} />
                    </div>
                  : <div className="shadow-inner-top border-t bg-white-sepia">
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