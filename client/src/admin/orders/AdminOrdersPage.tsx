import * as React from 'react';
import * as classNames from 'classnames'

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
                                        reconcilingQuantities: {[productId: string]: {householdId: number, householdName: string, quantity: number, minQuantity: number, maxQuantity: number, lastUpdated: number}[]}
                                      }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('order', collapsibleState => this.setState({collapsibleState})),
      addingHousehold: undefined,
      tab: 'households',
      reconcilingOrder: undefined,
      reconcilingQuantities: {}
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
    
    this.setState({reconcilingOrder: Util.clone(this.props.collectiveOrder), reconcilingQuantities: {}})
  }

  endReconcilingOrder = () => {
    if(!this.props.collectiveOrder) return
    
    this.setState({reconcilingOrder: undefined})
  }

  editItemQuantity = (item: Item, quantity: number) => {
    if(!this.state.reconcilingOrder) return

    if(!item.adjustment) {
      item.adjustment = {
        oldProductPriceExcVat: item.productPriceExcVat,
        oldProductPriceIncVat: item.productPriceIncVat,
        oldItemQuantity: item.itemQuantity,
        oldItemTotalExcVat: item.itemTotalExcVat,
        oldItemTotalIncVat: item.itemTotalIncVat,
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
        oldProductPriceExcVat: item.productPriceExcVat,
        oldProductPriceIncVat: item.productPriceIncVat,
        oldItemQuantity: item.itemQuantity,
        oldItemTotalExcVat: item.itemTotalExcVat,
        oldItemTotalIncVat: item.itemTotalIncVat,
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

  saveItem = (item: Item) => {
    if(!this.state.reconcilingOrder) return

    const reconcilingOrder = this.state.reconcilingOrder
    const reconcilingQuantities = this.state.reconcilingQuantities
    if(item.adjustment && item.itemQuantity != item.adjustment.oldItemQuantity && item.itemQuantity > 0) {
      const households: {householdId: number, householdName: string, quantity: number, minQuantity: number, maxQuantity: number, lastUpdated: number}[] = []
      let quantityRemaining = item.itemQuantity
      let lastUpdated = 0;
      for(let ho of reconcilingOrder.householdOrders) {
        let found = ho.items.find(i => i.productId === item.productId)
        if(!found) continue

        const oldQuantity = found.itemQuantity
        const quantity = Math.min(oldQuantity, quantityRemaining)
        quantityRemaining -= quantity
        households.push({householdId: ho.householdId, householdName: ho.householdName, quantity, minQuantity: 0, maxQuantity: Math.min(oldQuantity, item.itemQuantity), lastUpdated})
        lastUpdated++
      }
      if(households.length > 1) {
        for(let h of households) {
          let otherTotal = households.reduce((t, h2) => h2.householdId == h.householdId? t : t + h2.maxQuantity, 0)
          h.minQuantity = Math.max(0, item.itemQuantity - otherTotal)
        }
        reconcilingQuantities[item.productId] = households
      } else {
        item.reconciled = true
      }
    } else {
      item.reconciled = true
    }
    this.setState({reconcilingOrder, reconcilingQuantities})
  }

  editItem = (item: Item) => {
    if(!this.state.reconcilingOrder) return

    item.reconciled = false
    const reconcilingQuantities = this.state.reconcilingQuantities
    delete reconcilingQuantities[item.productId]
    this.setState({reconcilingOrder: this.state.reconcilingOrder, reconcilingQuantities})
  }

  updateQuantity = (item: Item, householdId: number, quantity: number) => {
    if(!this.state.reconcilingOrder) return
    
    const reconcilingQuantities = this.state.reconcilingQuantities
    const itemQuantities = reconcilingQuantities[item.productId]
    const household = itemQuantities.find(h => h.householdId == householdId)
    if(!household) return

    const oldQuantity = household.quantity
    if(quantity == oldQuantity) return

    household.quantity = quantity
    const oldLastUpdated = household.lastUpdated
    household.lastUpdated = 0

    let lastUpdatedIndexes: number[] = []
    for(let i = 0; i < itemQuantities.length; i++) {
      let h = itemQuantities[i]
      if(h != household && h.lastUpdated < oldLastUpdated) {
        h.lastUpdated++
      }

      lastUpdatedIndexes[h.lastUpdated] = i
    }

    let remainingQuantity = quantity - oldQuantity
    for(let i = lastUpdatedIndexes.length - 1; i >= 0 && Math.abs(remainingQuantity) > 0; i--) {
      let h = itemQuantities[lastUpdatedIndexes[i]]

      let oldQuantity = h.quantity
      h.quantity = Math.max(h.minQuantity, Math.min(h.maxQuantity, h.quantity - remainingQuantity))
      remainingQuantity += (h.quantity - oldQuantity)
    }

    this.setState({reconcilingQuantities})
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
                      {this.state.reconcilingOrder.items.map((item, index) => {
                        let maxQuantity = item.adjustment? item.adjustment.oldItemQuantity : item.itemQuantity
                        return <React.Fragment>
                          <OrderItem key={item.productId}
                                     item={item} 
                                     index={index}
                                     minQuantity={0}
                                     maxQuantity={maxQuantity}
                                     checkedOff={item.reconciled}
                                     editItemQuantity={!item.reconciled && !this.state.reconcilingQuantities[item.productId] && this.editItemQuantity || undefined}
                                     editProductPrice={!item.reconciled && !this.state.reconcilingQuantities[item.productId] && this.editProductPrice || undefined}
                                     saveItem={!item.reconciled && !this.state.reconcilingQuantities[item.productId] && this.saveItem || undefined}
                                     editItem={(item.reconciled || this.state.reconcilingQuantities[item.productId]) && this.editItem || undefined} />
                          {this.state.reconcilingQuantities[item.productId] &&
                            <DistributeQuantities item={item}
                                                  index={index}
                                                  reconcilingQuantities={this.state.reconcilingQuantities[item.productId]} 
                                                  updateQuantity={this.updateQuantity} />
                          }
                        </React.Fragment>
                      })}
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

export interface DistributeQuantitiesProps { item: Item
                                             index: number
                                             reconcilingQuantities: {householdId: number, householdName: string, quantity: number, maxQuantity: number, minQuantity: number}[]
                                             updateQuantity: (item: Item, householdId: number, quantity: number) => void
                                           }
export const DistributeQuantities = ({item, index, reconcilingQuantities, updateQuantity}: DistributeQuantitiesProps) => {
  return <React.Fragment>
    <tr>
      <td rowSpan={reconcilingQuantities.length + 1} className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})}>
      </td>
      <td colSpan={3} className={classNames('pb-2 pl-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        How do you want to distribute the items?
      </td>
    </tr>
    {reconcilingQuantities.map((h, ix) => {
      const quantities = []
      for(let i = h.minQuantity; i <= h.maxQuantity; i++) {
        quantities.push(i)
      }
      return <tr>
        <td colSpan={2} className={classNames('pb-2 pl-2 align-baseline')}>
          <span className="flex justify-between">
            <span>{h.householdName}</span>
            <span>
              <span className="line-through text-grey-darker mr-2">x {h.maxQuantity}</span>
              <select className="border" value={h.quantity} onChange={e=> updateQuantity(item, h.householdId, parseInt(e.target.value))}>
                {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
            </span>
          </span>
        </td>
        <td className={classNames('pl-2 pr-2 align-bottom text-right')}>
          {ix === reconcilingQuantities.length - 1 &&
            <button className="ml-4 whitespace-no-wrap" onClick={() => {}}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
          }
        </td>
      </tr>
    })}
  </React.Fragment>
}