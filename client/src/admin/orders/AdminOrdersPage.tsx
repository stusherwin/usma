import * as React from 'react';

import { CollectiveOrder, Household, OrderItem as Item } from 'util/Types'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'
import { Router } from 'util/Router'
import { Icon } from 'util/Icon'
import { Util } from 'util/Util'

import { OrderTabs } from 'order/OrderTabs'
import { OrderItems } from 'order/OrderItems'
import { OrderTotal } from 'order/OrderTotal'
import { OrderStatus } from 'order/OrderStatus'
import { OrderItem } from 'order/OrderItem'
import { OrderFooter } from 'order/OrderFooter'

import { AdminTopNav } from 'admin/AdminTopNav'

import { PastCollectiveOrders } from './PastCollectiveOrders'
import { HouseholdOrders } from './HouseholdOrders'
import { ProductCodes } from './ProductCodes'
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { CollectiveOrderMessages } from './CollectiveOrderMessages'

export interface AdminOrdersPageProps { collectiveOrder: CollectiveOrder | undefined
                                      , pastOrders: CollectiveOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

export interface AdminOrdersPageState { collapsibleState: CollapsibleState 
                                        addingHousehold: Household | undefined
                                        tab: 'households' | 'product-list' | 'product-codes'
                                        reconcilingOrder: CollectiveOrder | undefined
                                      }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('order', collapsibleState => this.setState({collapsibleState})),
      addingHousehold: undefined,
      tab: 'households',
      reconcilingOrder: Util.clone(this.props.collectiveOrder)
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
    
    this.setState({reconcilingOrder: Util.clone(this.props.collectiveOrder)})
  }

  endReconcilingOrder = () => {
    if(!this.props.collectiveOrder) return
    
    this.setState({reconcilingOrder: undefined})
  }

  editItemQuantity = (item: Item, quantity: number) => {
    if(!this.state.reconcilingOrder) return

    if(!item.adjustment) {
      item.adjustment = {
        oldItemTotalExcVat: item.itemTotalExcVat,
        oldItemTotalIncVat: item.itemTotalIncVat,
        oldProductPriceExcVat: item.productPriceExcVat,
        oldProductPriceIncVat: item.productPriceIncVat,
        productDiscontinued: false
      }
    }
    item.itemQuantity = quantity
    item.itemTotalExcVat = item.productPriceExcVat * item.itemQuantity
    item.itemTotalIncVat = item.productPriceIncVat * item.itemQuantity

    if(!this.state.reconcilingOrder.adjustment) {
      this.state.reconcilingOrder.adjustment = {
        oldTotalExcVat: this.state.reconcilingOrder.totalExcVat,
        oldTotalIncVat: this.state.reconcilingOrder.totalIncVat
      }
    }
    this.state.reconcilingOrder.totalExcVat = this.state.reconcilingOrder.items.reduce((t, i) => t + i.itemTotalExcVat, 0)
    this.state.reconcilingOrder.totalIncVat = this.state.reconcilingOrder.items.reduce((t, i) => t + i.itemTotalIncVat, 0)
    this.setState({reconcilingOrder: this.state.reconcilingOrder})
  }

  editProductPrice = (item: Item, price: number) => {
    if(!this.state.reconcilingOrder) return
    
    if(!item.adjustment) {
      item.adjustment = {
        oldItemTotalExcVat: item.itemTotalExcVat,
        oldItemTotalIncVat: item.itemTotalIncVat,
        oldProductPriceExcVat: item.productPriceExcVat,
        oldProductPriceIncVat: item.productPriceIncVat,
        productDiscontinued: false
      }
    }
    const diff = price - item.productPriceExcVat
    item.productPriceExcVat = price
    item.productPriceIncVat += diff
    item.itemTotalExcVat = item.productPriceExcVat * item.itemQuantity
    item.itemTotalIncVat = item.productPriceIncVat * item.itemQuantity
    
    if(!this.state.reconcilingOrder.adjustment) {
      this.state.reconcilingOrder.adjustment = {
        oldTotalExcVat: this.state.reconcilingOrder.totalExcVat,
        oldTotalIncVat: this.state.reconcilingOrder.totalIncVat
      }
    }
    this.state.reconcilingOrder.totalExcVat = this.state.reconcilingOrder.items.reduce((t, i) => t + i.itemTotalExcVat, 0)
    this.state.reconcilingOrder.totalIncVat = this.state.reconcilingOrder.items.reduce((t, i) => t + i.itemTotalIncVat, 0)
    this.setState({reconcilingOrder: this.state.reconcilingOrder})
  }

  render() {
    const order = this.props.collectiveOrder

    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <Collapsible className="min-h-20"
                     collapsibleKey="order"
                     collapsibleState={this.state.collapsibleState}
                     {...this.props}
                     header={
                       <div className="p-2 bg-order-dark min-h-20">
                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                         <h2 className="leading-none ml-20 relative flex">
                           Current order
                         </h2>
                         <h3 className="flex justify-between ml-20 mt-4">
                           <OrderStatus order={order} />
                           <OrderTotal order={order} />
                         </h3>
                         {!this.state.reconcilingOrder &&
                           <CollectiveOrderButtons order={order}
                                                   newOrder={this.newOrder} 
                                                   deleteOrder={this.deleteOrder} 
                                                   abandonOrder={this.abandonOrder} 
                                                   placeOrder={this.placeOrder}
                                                   reconcileOrder={this.startReconcilingOrder} />
                         }
                         {!!order && !this.state.reconcilingOrder &&
                           <div className="mt-4">
                             <OrderTabs tab={this.state.tab} setTab={tab => this.setState({tab})} />
                           </div>
                         }
                       </div>
                     }>
          { order && (
            this.state.reconcilingOrder?
              <div className="">
                <div className="bg-product-light text-white p-2 relative shadow-inner-top">
                  <div className="bg-img-product bg-no-repeat w-16 h-16 absolute"></div>
                  <h2 className="leading-none ml-20">Reconcile order</h2>
                  <div className="ml-20 mt-3">
                    <button onClick={this.endReconcilingOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Done</button>
                  </div>
                </div>
                <div className="shadow-inner-top border-t bg-white">
                  <table className="border-collapse w-full">
                    <tbody>
                      {this.state.reconcilingOrder.items.map((item, index) => 
                        <OrderItem key={item.productId}
                                   item={item} 
                                   index={index}
                                   allowZeroQuantity={true}
                                   editItemQuantity={this.editItemQuantity}
                                   editProductPrice={this.editProductPrice} />
                      )}
                      <OrderFooter order={this.state.reconcilingOrder} />
                    </tbody>
                  </table>
                </div>
              </div>
            : this.state.tab == 'households'?
              <div className="shadow-inner-top border-t bg-household-lightest">
                <CollectiveOrderMessages order={order} />
                <div className="flex justify-end mt-4 mr-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/household-orders-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                </div>
                <HouseholdOrders order={order}
                                 {...this.props} />
              </div>
            : this.state.tab == 'product-list'?
              <div className="shadow-inner-top border-t bg-white">
                <CollectiveOrderMessages order={order} />
                <div className="flex justify-end mr-2 mt-4 mb-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
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