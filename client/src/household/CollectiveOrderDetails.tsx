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
    if(!this.props.household.currentHouseholdOrder) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.household.currentHouseholdOrder.orderId, this.props.household.id, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.collectiveOrder)
      return

    const orderId = this.props.collectiveOrder.id
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
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

  leaveOrder = () => {
    if(!this.props.household.currentHouseholdOrder)
      return

    this.props.request(ServerApi.command.deleteHouseholdOrder(this.props.household.currentHouseholdOrder.orderId, this.props.household.id))
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
      <Collapsible className="min-h-20"
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   onCollapse={this.cancelAdd}
                   {...this.props}
                   header={
                     <div className="p-2 bg-order-dark min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Current order
                       </h2>
                       <div>
                         <h3 className="flex justify-between ml-20 mt-4 mb-2">
                           <OrderStatus order={householdOrder} />
                           <OrderTotal order={householdOrder} />
                         </h3>
                         {!this.state.addingProduct &&
                           <HouseholdOrderButtons unusedProducts={unusedProducts} 
                                                         currentHouseholdOrder={householdOrder} 
                                                         leaveOrder={this.leaveOrder} 
                                                         reopenOrder={this.reopenOrder}
                                                         abandonOrder={this.abandonOrder}
                                                         completeOrder={this.completeOrder}
                                                         startAdd={this.startAdd} />
                         }
                       </div>
                     </div>
                   }>
        <div className="shadow-inner-top bg-white">
        { !order? 
          <div className="px-2 py-4 text-grey-darker">
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
            <button className="mt-4" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
          </div>
        : !householdOrder?
          <div className="px-2 py-4 text-grey-darker">
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><strong>{order.createdBy == this.props.household.id ? 'You' : order.createdByName}</strong> started an order on <strong>{Util.formatDate(order.createdDate)}</strong></p  >
            <button className="mt-4" onClick={this.joinOrder}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
          </div>
        : this.state.addingProduct?
          <AddProduct products={unusedProducts}
                      cancelAdd={this.cancelAdd}
                      confirmAdd={this.confirmAdd}
                      {...this.props} />
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