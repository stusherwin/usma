import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../Types'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { Collapsible } from '../common/Collapsible'
import { HouseholdOrders } from './HouseholdOrdersForOrder'
import { CurrentOrderItems } from './CurrentOrderItems'
import { CurrentOrderProductCodes } from './CurrentOrderProductCodes'

export interface CurrentOrderProps { currentOrder: CollectiveOrder | null
                                   , currentHouseholdOrders: HouseholdOrder[]
                                   , households: Household[]
                                   , expanded: boolean
                                   , otherExpanding: boolean
                                   , toggle: () => void
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { addingHousehold: Household | null
                                   , tab: 'households' | 'product-list' | 'product-codes'
                                   }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  constructor(props: CurrentOrderProps) {
    super(props)

    this.state = { addingHousehold: null
                 , tab: 'households'
                 }
  }

  newOrder = () => {
    // TODO: Have some way of choosing the household to create order
    this.props.request(ServerApi.command.createOrder(this.props.households[0].id))
      .then(this.props.reload)
  }

  deleteOrder = () => {
    if(!this.props.currentOrder) return

    this.props.request(ServerApi.command.deleteOrder(this.props.currentOrder.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/admin/orders`))
  }

  abandonOrder = () => {
    if(!this.props.currentOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.currentOrder.id))
      .then(this.props.reload)
  }

  placeOrder = () => {
    if(!this.props.currentOrder) return

    this.props.request(ServerApi.command.placeOrder(this.props.currentOrder.id))
      .then(this.props.reload)
  }

  render() {
    const currentOrder = this.props.currentOrder

    return (
      <Collapsible className="min-h-20"
                   {...this.props}
                   header={() => 
                     <div className="p-2 bg-order-dark min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Current order
                       </h2>
                       <h3 className="flex justify-between ml-20 mt-4 mb-2">
                         {this.renderStatus()}
                         <span className="flex justify-end">
                           <span>Total:</span>
                           <span className="w-24 font-bold text-right">{this.renderTotal()}</span>
                         </span>
                       </h3>
                       {this.renderButtons()}
                     </div>
                   }>
        <div className="bg-white shadow-inner-top border-t">
          { !currentOrder?
            <div className="px-2 py-4">
              <div className="my-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order currently in progress</div>
              <div className="flex justify-start">
                <button onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New order</button>
              </div>
            </div>
          : <div>
              <div className="pt-2 flex bg-grey-light shadow-inner-top">
                <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'households'})}} className={classNames("flex-grow text-center p-2 no-underline text-black hover:text-black ml-1 rounded-t-lg",      { "bg-white": this.state.tab == 'households', "bg-grey-lightest shadow-inner-bottom": this.state.tab != 'households' })}>Households</a>
                <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-list'})}} className={classNames("flex-grow text-center p-2 no-underline text-black hover:text-black ml-1 rounded-t-lg",      { "bg-white": this.state.tab == 'product-list', "bg-grey-lightest shadow-inner-bottom": this.state.tab != 'product-list' })}>Product list</a>
                <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-codes'})}} className={classNames("flex-grow text-center p-2 no-underline text-black hover:text-black ml-1 rounded-t-lg mr-1", { "bg-white": this.state.tab == 'product-codes', "bg-grey-lightest shadow-inner-bottom": this.state.tab != 'product-codes' })}>Product codes</a>
              </div>
              {this.renderMessages()}
              {this.state.tab == 'households' && (
                !this.props.currentHouseholdOrders.length?
                  <div className="px-2 py-4 text-grey-darker">
                    <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order yet
                  </div>
                : <HouseholdOrders order={currentOrder}
                                   householdOrders={this.props.currentHouseholdOrders}
                                   households={this.props.households}
                                   reload={this.props.reload}
                                   request={this.props.request} />
              )}
              {this.state.tab == 'product-list' && 
                <CurrentOrderItems currentOrder={currentOrder} />
              }
              {this.state.tab == 'product-codes' && 
                <CurrentOrderProductCodes currentOrder={currentOrder} />
              }
          </div>
          }
        </div>
      </Collapsible>
    )
  }

  renderStatus = () => {
    return (
      <span>
        {!this.props.currentOrder?
          <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Available</span>
        : this.props.currentOrder.isComplete?
          <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
        : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />In progress</span>
        }
      </span>
    )
  }
  
  renderTotal = () => {
    const order = this.props.currentOrder
    return !order?
      <Money amount={0} />
    : order.oldTotalIncVat === null || order.oldTotalIncVat == order.totalIncVat?
      <Money amount={order.totalIncVat} />
    : <span>
        <span className="line-through"><Money amount={order.oldTotalIncVat} /></span> 
        <Money className="text-red font-bold" amount={order.totalIncVat} />
      </span>
  }

  renderMessages = () => {
    const order = this.props.currentOrder
    const householdOrders = this.props.currentHouseholdOrders

    const allComplete = householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const orderMinimumReached = order && order.totalIncVat >= 25000
    const allHouseholdsUpToDate = !!this.props.currentOrder && this.props.currentOrder.allHouseholdsUpToDate;

    return (
      <div className="flex text-black px-2 py-4">
        <Icon type={householdOrders.length && allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 fill-current mr-2 nudge-d-2" />
        { !householdOrders.length?
          <span>Waiting for households to join</span>
        : !allHouseholdsUpToDate?
          <span>Waiting for all households to accept latest catalogue updates</span>
        : !orderMinimumReached?
          <span>Waiting for &pound;250.00 order minimum</span>
        : !allComplete?
          <span>Waiting for all orders to be completed</span>
          // : !allPaid?
          //   <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
        : <span>Order can now be placed</span>
        }
      </div>
    )
  }

  renderButtons = () => {
    const order = this.props.currentOrder
    if(!order) return;

    const householdOrders = this.props.currentHouseholdOrders

    const allComplete = householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const orderMinimumReached = order && order.totalIncVat >= 25000

    const deleteOrderPossible = !householdOrders.length
    const placeOrderPossible = !!householdOrders.length
    const placeOrderAllowed = allComplete /*&& allPaid*/ && orderMinimumReached
    const abandonOrderPossible = !!householdOrders.length

    return (
      <div className="flex flex-wrap content-start items-start">
        {deleteOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); this.deleteOrder() }}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
        }
        {abandonOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); this.abandonOrder() }}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Abandon order</button>
        }
        {placeOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!placeOrderAllowed} onClick={e => {e.preventDefault(); e.stopPropagation(); this.placeOrder()}}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
        }
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); document.location.href = ServerApi.url("query/collective-order-download/")}}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
    </div>
    )
  }
}