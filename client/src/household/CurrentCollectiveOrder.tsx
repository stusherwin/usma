import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, CollectiveOrder, ProductCatalogueEntry } from '../util/Types'
import { Util } from '../util/Util'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'
import { Collapsible, CollapsibleState } from '../util/Collapsible'
import { ServerApi } from '../util/ServerApi'

import { AddProduct } from './AddProduct'
import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'

export interface CurrentCollectiveOrderProps { household: Household
                                   , collectiveOrder: CollectiveOrder | null
                                   , households: Household[]
                                   , products: ProductCatalogueEntry[]
                                   , categories: string[]
                                   , brands: string[]
                                   , collapsibleKey: string
                                   , collapsibleState: CollapsibleState
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentCollectiveOrderState { addingProduct: boolean
                                   }

export class CurrentCollectiveOrder extends React.Component<CurrentCollectiveOrderProps, CurrentCollectiveOrderState> {
  constructor(props: CurrentCollectiveOrderProps) {
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
    if(!this.props.household.currentHouseholdOrder)
      return

    const orderId = this.props.household.currentHouseholdOrder.orderId
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
                           {this.renderStatus()}
                           <span className="flex justify-end">
                             {/* <span>Total:</span> */}
                             <span className="w-24 text-right">{this.renderTotal()}</span>
                           </span>
                         </h3>
                         {this.renderButtons(unusedProducts)}
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
            <button className="mt-4" onClick={_ => this.joinOrder()}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
          </div>
        : this.state.addingProduct?
          <AddProduct products={unusedProducts}
                      cancelAdd={this.cancelAdd}
                      confirmAdd={this.confirmAdd}
                      {...this.props} />
        : !householdOrder.items.length?
          <div className="px-2 py-4 text-grey-darker">
            <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet {!this.props.products.length && ' - the product catalogue is empty'}
          </div>
        : <div>
            {this.renderMessages()}
            <CurrentHouseholdOrder currentHouseholdOrder={householdOrder}
                                   reload={this.props.reload}
                                   request={this.props.request} />
          </div>
        }
        </div>
      </Collapsible>
    )
  }

  renderStatus = () => {
    return (
      <span>
        {!this.props.household.currentHouseholdOrder?
          <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Available</span>
        : this.props.household.currentHouseholdOrder.isComplete?
          <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
        : this.props.household.currentHouseholdOrder.isAbandoned?
          <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Abandoned</span>
        : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
        }
      </span>
    )
  }

  renderTotal = () => {
    let householdOrder = this.props.household.currentHouseholdOrder

    return !householdOrder?
      <Money amount={0} />
    : householdOrder.oldTotalIncVat === null || householdOrder.oldTotalIncVat == householdOrder.totalIncVat?
      <Money className={classNames({"line-through text-grey-darker": householdOrder.isAbandoned})} amount={householdOrder.totalIncVat} />
    : <span>
        <span className="line-through"><Money amount={householdOrder.oldTotalIncVat} /></span> 
        <Money className="text-red font-bold" amount={householdOrder.totalIncVat} />
      </span>
  }

  renderButtons = (unusedProducts: ProductCatalogueEntry[]) => {
    let householdOrder = this.props.household.currentHouseholdOrder
    if(!householdOrder) return

    if(this.state.addingProduct) return

    const canAddItem = householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = !householdOrder.items.length
    const canReopenOrder = !!householdOrder.items.length && !householdOrder.isOpen
    const canAbandonOrder = !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

    if(!orderButtons.some(b => b)) return

    return (
      <div className="flex flex-wrap content-start items-start">
        {canLeaveOrder && 
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); this.leaveOrder()}}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</button>
        }
        {canReopenOrder &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); this.reopenOrder()}}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</button>
        }
        {canAbandonOrder &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); this.abandonOrder()}}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon</button>
        }
        {canCompleteOrder && 
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); this.completeOrder()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete</button>
        }
        {canAddItem &&
          <button className="flex-no-grow flex-no-shrink ml-auto mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); this.startAdd()}}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add items</button>
        }
      </div>
    )
  }
  
  renderMessages = () => {
    let householdOrder = this.props.household.currentHouseholdOrder
    if(!householdOrder) return

    const allComplete = !!this.props.collectiveOrder && this.props.collectiveOrder.householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    // const householdsInOrder = this.props.households.filter(h => !!this.props.currentHouseholdOrders.find(oh => oh.householdId == h.id))
    // const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)
    const allHouseholdsUpToDate = !!this.props.collectiveOrder && this.props.collectiveOrder.allHouseholdsUpToDate;
    const orderMinimumReached = !!this.props.collectiveOrder && this.props.collectiveOrder.totalIncVat >= 25000

    if(!!householdOrder.oldTotalExcVat && householdOrder.oldTotalIncVat != householdOrder.totalIncVat)
      return (
        <div className="flex bg-red-lighter p-2 mb-4">
          <Icon type="alert" className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-2" />The product catalogue was updated and your order has been affected. Please review and accept the changes before continuing.
          <div className="flex justify-end mt-2"><button onClick={e => {e.stopPropagation(); this.acceptUpdates()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Accept changes</button></div>
        </div>
      )

    if(!householdOrder.isComplete)
      return

    return (
      <div className="flex px-2 py-4 text-black">
        <Icon type={allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-2" />
        { !allHouseholdsUpToDate?
          <span>Waiting for all households to accept latest catalogue updates</span>
        : !orderMinimumReached?
          <span>Waiting for minimum order to be reached. Current total is <Money amount={!!this.props.collectiveOrder && this.props.collectiveOrder.totalIncVat || 0} /> of &pound;250.00</span>
        : !allComplete?
          <span>Waiting for all orders to be completed</span>
        // : !allPaid?
        //   <span>, waiting for everyone to pay up</span>
        : <span>Waiting for admin to place order</span>
        }
      </div>
    )
  }
}