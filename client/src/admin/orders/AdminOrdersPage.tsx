import * as React from 'react'

import { CollectiveOrder, Household } from 'util/Types'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'
import { Router } from 'util/Router'
import { Icon } from 'util/Icon'

import { OrderTabs } from 'order/OrderTabs'
import { OrderItems } from 'order/OrderItems'
import { OrderTotal } from 'order/OrderTotal'
import { OrderStatus } from 'order/OrderStatus'

import { AdminTopNav } from 'admin/AdminTopNav'

import { PastCollectiveOrders } from './PastCollectiveOrders'
import { HouseholdOrders } from './HouseholdOrders'
import { ProductCodes } from './ProductCodes'
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { CollectiveOrderMessages } from './CollectiveOrderMessages'
import { ReconcileOrder } from './ReconcileOrder'

export interface AdminOrdersPageProps { collectiveOrder: CollectiveOrder | undefined
                                        pastOrders: CollectiveOrder[]
                                        households: Household[]
                                        request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        reload: () => Promise<void>
                                      }

export interface AdminOrdersPageState { collapsibleState: CollapsibleState 
                                        addingHousehold: Household | undefined
                                        tab: 'households' | 'product-list' | 'product-codes'
                                        reconcilingOrder: boolean
                                      }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('order', collapsibleState => this.setState({collapsibleState})),
      addingHousehold: undefined,
      tab: 'households',
      reconcilingOrder: false,
    }
  }

  newOrder = () => {
    // TODO: Have some way of choosing the household to create order
    this.props.request(ServerApi.command.createOrder(this.props.households[0].id))
      .then(this.props.reload)
  }

  deleteOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.deleteOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/admin/orders`))
  }

  abandonOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  placeOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.placeOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  startReconcilingOrder = () => {
    if(!this.props.collectiveOrder) return

    this.setState({reconcilingOrder: true})
  }

  endReconcilingOrder = () => {
    if(!this.state.reconcilingOrder) return
    
    this.setState({reconcilingOrder: false})
  }

  endReconcilingItem = (productId: number, productPriceExcVat: number, households: {householdId: number, itemQuantity: number}[]) => {
    if(!this.state.reconcilingOrder) return
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.reconcileOrderItem(this.props.collectiveOrder.id, productId, productPriceExcVat, households))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.collectiveOrder

    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <Collapsible className="min-h-24"
                     collapsibleKey="order"
                     collapsibleState={this.state.collapsibleState}
                     onCollapsed={this.endReconcilingOrder}
                     {...this.props}
                     header={
                       <div className="p-2 pt-4 bg-order-dark h-24">
                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                         <div className="flex justify-between">
                           <h2 className="leading-none ml-20">
                             Current order
                           </h2>
                           <h3>
                             <OrderTotal order={order} />
                           </h3>
                         </div>
                         <h3 className="flex justify-between ml-20 mt-4">
                           <OrderStatus order={order} />
                         </h3>
                       </div>
                     }
                     expandedHeader={
                       <div className="p-2 bg-order-dark -mt-4">
                         {!this.state.reconcilingOrder &&
                           <CollectiveOrderButtons order={order}
                                                   newOrder={this.newOrder} 
                                                   deleteOrder={this.deleteOrder} 
                                                   abandonOrder={this.abandonOrder} 
                                                   placeOrder={this.placeOrder}
                                                   reconcileOrder={this.startReconcilingOrder} />
                         }
                         {!!order && !this.state.reconcilingOrder &&
                           <div className="mt-5">
                             <OrderTabs tab={this.state.tab} setTab={tab => this.setState({tab})} />
                           </div>
                         }
                       </div>
                     }>
          { order && (
            this.state.reconcilingOrder?
              <ReconcileOrder order={order} 
                              endReconcilingOrder={this.endReconcilingOrder}
                              endReconcilingItem={this.endReconcilingItem} />
            : this.state.tab == 'households'?
              <div className="shadow-inner-top border-t bg-household-lightest">
                <CollectiveOrderMessages order={order} />
                <div className="flex justify-end mt-4 mr-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url("query/household-orders-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                </div>
                <HouseholdOrders order={order}
                                 {...this.props} />
              </div>
            : this.state.tab == 'product-list'?
              <div className="shadow-inner-top border-t bg-white">
                <CollectiveOrderMessages order={order} />
                <div className="flex justify-end mr-2 mt-4 mb-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                </div>
                <OrderItems order={order} />
              </div>
            : <div className="shadow-inner-top border-t bg-white">
                <CollectiveOrderMessages order={order} />
                <ProductCodes order={order} />
              </div>
            )
          }
        </Collapsible>
        <PastCollectiveOrders collapsibleKey="past-orders"
                              collapsibleState={this.state.collapsibleState}
                              {...this.props} />
      </div>
    )
  }
}