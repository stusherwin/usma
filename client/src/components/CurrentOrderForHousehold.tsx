import * as React from 'react';
import * as classNames from 'classnames'

import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { Household, HouseholdOrder, CollectiveOrder, ProductCatalogueEntry, OrderItem } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible } from '../common/Collapsible'
import { ServerApi } from '../ServerApi'
import { AddProduct } from './AddProduct'

export interface CurrentOrderProps { household: Household
                                   , currentOrder: CollectiveOrder | null
                                   , currentHouseholdOrders: HouseholdOrder[]
                                   , currentHouseholdOrder: HouseholdOrder | null
                                   , products: ProductCatalogueEntry[]
                                   , households: Household[]
                                   , expanded: boolean
                                   , otherExpanding: boolean
                                   , toggle: () => void
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { addingProduct: boolean
                                   }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  constructor(props: CurrentOrderProps) {
    super(props)

    this.state = { addingProduct: false
                 }
  }

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () => this.setState({ addingProduct: false })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if(!this.state.addingProduct) return Promise.resolve();
    if(!this.props.currentHouseholdOrder) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.currentOrder)
      return

    const orderId = this.props.currentOrder.id
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }

  abandonOrder = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.abandonHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  completeOrder = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.completeHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  reopenOrder = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.reopenHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  leaveOrder = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.deleteHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  unusedProducts = () => {
    if(!this.props.currentHouseholdOrder) return []

    const items = this.props.currentHouseholdOrder.items
    return this.props.products.filter(p => !items.find(i => i.productCode == p.code))
  }

  acceptUpdates = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.acceptCatalogueUpdates(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  render() {
    let order = this.props.currentOrder
    let householdOrder = this.props.currentHouseholdOrder
    let unusedProducts = this.unusedProducts()

    return (
      <Collapsible className="min-h-20"
                   onCollapse={this.cancelAdd}
                   {...this.props}
                   header={() => 
                     <div className="p-2 bg-order-dark min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Current order
                       </h2>
                       <div>
                         <h3 className="flex justify-between ml-20 mt-4 mb-2">
                           {this.renderStatus()}
                           <span className="flex justify-end">
                             <span>Total:</span>
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
                      confirmAdd={this.confirmAdd} />
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
        {!this.props.currentHouseholdOrder?
          <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Available</span>
        : this.props.currentHouseholdOrder.isComplete?
          <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
        : this.props.currentHouseholdOrder.isAbandoned?
          <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Abandoned</span>
        : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
        }
      </span>
    )
  }

  renderTotal = () => {
    let householdOrder = this.props.currentHouseholdOrder

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
    let householdOrder = this.props.currentHouseholdOrder
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
    let householdOrder = this.props.currentHouseholdOrder
    if(!householdOrder) return

    const allComplete = this.props.currentHouseholdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    // const householdsInOrder = this.props.households.filter(h => !!this.props.currentHouseholdOrders.find(oh => oh.householdId == h.id))
    // const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)
    const allHouseholdsUpToDate = !!this.props.currentOrder && this.props.currentOrder.allHouseholdsUpToDate;
    const orderMinimumReached = !!this.props.currentOrder && this.props.currentOrder.totalIncVat >= 25000

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
          <span>Waiting for minimum order to be reached. Current total is <Money amount={!!this.props.currentOrder && this.props.currentOrder.totalIncVat || 0} /> of &pound;250.00</span>
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