import * as React from 'react';

import { Household, CollectiveOrder, ProductCatalogueEntry } from 'util/Types'
import { Util } from 'util/Util'
import { Icon } from 'util/Icon'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'

import { AddProduct } from './AddProduct'
import { HouseholdOrderItems } from './HouseholdOrderItems'
import { HouseholdOrderButtons } from './HouseholdOrderButtons'
import { CollectiveOrderMessages } from './CollectiveOrderMessages'

export interface CollectiveOrderDetailsProps { household: Household
                                               collectiveOrder: CollectiveOrder | undefined
                                               households: Household[]
                                               products: ProductCatalogueEntry[]
                                               categories: string[]
                                               brands: string[]
                                               collapsibleKey: string
                                               collapsibleState: CollapsibleState
                                               request: <T extends {}>(p: Promise<T>) => Promise<T>
                                               reload: () => Promise<void>
                                             }

export interface CollectiveOrderDetailsState { addingProduct: boolean
                                             }

export class CollectiveOrderDetails extends React.Component<CollectiveOrderDetailsProps, CollectiveOrderDetailsState> {
  constructor(props: CollectiveOrderDetailsProps) {
    super(props)

    this.state = { addingProduct: false
                 }
  }

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () => this.setState({ addingProduct: false })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if(!this.state.addingProduct) return Promise.resolve();
    if(!this.props.collectiveOrder) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.collectiveOrder.id, this.props.household.id, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  abandonOrder = () => {
    if(!this.props.household.currentHouseholdOrder)
      return

    this.props.request(ServerApi.command.abandonHouseholdOrder(this.props.household.currentHouseholdOrder.orderId, this.props.household.id))
      .then(this.props.reload)
  }

  completeOrder = () => {
    if(!this.props.household.currentHouseholdOrder)
      return

    this.props.request(ServerApi.command.completeHouseholdOrder(this.props.household.currentHouseholdOrder.orderId, this.props.household.id))
      .then(this.props.reload)
  }

  reopenOrder = () => {
    if(!this.props.household.currentHouseholdOrder)
      return

    this.props.request(ServerApi.command.reopenHouseholdOrder(this.props.household.currentHouseholdOrder.orderId, this.props.household.id))
      .then(this.props.reload)
  }

  unusedProducts = () => {
    if(!this.props.household.currentHouseholdOrder)
      return []

    const items = this.props.household.currentHouseholdOrder.items
    return this.props.products.filter(p => !items.find(i => i.productCode == p.code))
  }

  acceptUpdates = () => {
    if(!this.props.household.currentHouseholdOrder)
      return

    this.props.request(ServerApi.command.acceptCatalogueUpdates(this.props.household.currentHouseholdOrder.orderId, this.props.household.id))
      .then(this.props.reload)
  }

  render() {
    let order = this.props.collectiveOrder
    let householdOrder = this.props.household.currentHouseholdOrder
    let unusedProducts = this.unusedProducts()

    return (
      <Collapsible collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   onCollapse={this.cancelAdd}
                   {...this.props}
                   header={
                     <div className="p-2 pt-4 bg-order-dark min-h-24">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <div className="flex justify-between items-baseline ml-20">
                         <div>
                           <h2 className="leading-none">
                             Current order
                           </h2>
                           <h3 className="mt-4">
                             <OrderStatus order={householdOrder || order} />
                           </h3>
                         </div>
                         <h3 className="ml-2">
                           <OrderTotal order={householdOrder} />
                         </h3>
                       </div>
                     </div>
                   }
                   expandedHeader={!this.state.addingProduct &&
                     <HouseholdOrderButtons className="p-2 bg-order-dark -mt-4"
                                            unusedProducts={unusedProducts} 
                                            currentHouseholdOrder={householdOrder} 
                                            newOrder={this.newOrder} 
                                            reopenOrder={this.reopenOrder}
                                            abandonOrder={this.abandonOrder}
                                            completeOrder={this.completeOrder}
                                            startAdd={this.startAdd} />
                   || undefined}>
        <div className="shadow-inner-top bg-white">
        { !order? 
          <div className="px-2 py-4 text-black">
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
            <button className="mt-4" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
          </div>
        : this.state.addingProduct?
          <AddProduct products={unusedProducts}
                      cancelAdd={this.cancelAdd}
                      confirmAdd={this.confirmAdd}
                      {...this.props} />
        : !householdOrder?
          <div className="px-2 py-4 text-black">
            <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items {!!this.props.products && !this.props.products.length && ' - the product catalogue is empty'}
          </div>
        : <div>
            <CollectiveOrderMessages householdOrder={householdOrder} 
                                     collectiveOrder={order} 
                                     acceptUpdates={this.acceptUpdates} />
            <HouseholdOrderItems householdOrder={householdOrder}
                                 {...this.props} />
          </div>
        }
        </div>
      </Collapsible>
    )
  }
}