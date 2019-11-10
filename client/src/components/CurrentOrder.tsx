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
    const order = this.props.currentOrder
    const householdOrders = this.props.currentHouseholdOrders

    return (
      <Collapsible className="min-h-20"
                   {...this.props}
                   header={() => 
                     <div className="p-2 bg-order-dark min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Current order
                       </h2>
                       <h3 className="flex justify-between ml-20 mt-4">
                         <CurrentOrderStatus order={order} householdOrders={householdOrders} />
                         <CurrentOrderTotal order={order} householdOrders={householdOrders} />
                       </h3>
                       <CurrentOrderButtons order={order} householdOrders={householdOrders}
                          newOrder={this.newOrder} deleteOrder={this.deleteOrder} abandonOrder={this.abandonOrder} placeOrder={this.placeOrder} />
              <CurrentOrderTabs order={order} tab={this.state.tab} setTab={tab => this.setState({tab})} />
                     </div>
                   }>
          { order && 
            <div className={classNames("shadow-inner-top border-t", {
              "bg-household-lightest": this.state.tab == 'households',
              "bg-white": this.state.tab != 'households'
            })}>
              <CurrentOrderMessages order={order} householdOrders={householdOrders} />
              { this.state.tab == 'households'?
                <HouseholdOrders order={order}
                                 householdOrders={householdOrders}
                                 households={this.props.households}
                                 reload={this.props.reload}
                                 request={this.props.request} />
              : this.state.tab == 'product-list'?
                <CurrentOrderItems currentOrder={order} />
              : <CurrentOrderProductCodes currentOrder={order} />
              }
            </div>
          }
      </Collapsible>
    )
  }
}

interface Props {
  order: CollectiveOrder | null
  householdOrders: HouseholdOrder[]
}

interface CurrentOrderTabsProps {
  order: CollectiveOrder | null
  tab: 'households' | 'product-list' | 'product-codes'
  setTab: (tab: 'households' | 'product-list' | 'product-codes') => void
}

const CurrentOrderTabs = ({order, tab, setTab}: CurrentOrderTabsProps) => {
  if(!order) {
    return <span></span>;
  }

  /* <div className="flex justify-end">
    <div className="p-1 border border-order-darker rounded-full flex justify-end items-baseline overflow-hidden bg-grey-lighter shadow-inner-top">
      <a href="#" className={classNames("px-2 py-1 rounded-full text-xs uppercase tracking-wide no-underline text-black hover:text-black hover:no-underline hover:bg-order-dark hover:border hover:border-order-darker", { 
        "bg-order-dark border border-order-darker": this.state.tab == 'households', 
        "b": this.state.tab != 'households' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'households'})}}>Households</a>
      <a href="#" className={classNames("px-2 py-1 rounded-full text-xs uppercase tracking-wide uppercase no-underline text-black hover:text-black hover:no-underline hover:bg-order-dark hover:border hover:border-order-darker", { 
        "bg-order-dark border border-order-darker": this.state.tab == 'product-list', 
        "b": this.state.tab != 'product-list' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-list'})}}>Product list</a>
      <a href="#" className={classNames("px-2 py-1 rounded-full text-xs uppercase tracking-wide uppercase no-underline text-black hover:text-black hover:no-underline hover:bg-order-dark hover:border hover:border-order-darker", {
        "bg-order-dark border border-order-darker": this.state.tab == 'product-codes', 
        "b": this.state.tab != 'product-codes' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-codes'})}}>Product codes</a>
    </div>
  </div> */
  /* <div className="flex justify-end">
    View: 
    <input type="radio" name="tab" id="households" checked={this.state.tab == 'households'} className="ml-4 mr-1 nudge-d-1"
           onClick={e => e.stopPropagation() } onChange={e => { if(e.target.checked) { this.setState({tab: 'households'}); }}} />
    <label onClick={e => e.stopPropagation() } htmlFor="households">Households</label>
    <input type="radio" name="tab" id="product-list" checked={this.state.tab == 'product-list'} className="ml-4 mr-1 nudge-d-1" 
           onClick={e => e.stopPropagation() } onChange={e => { if(e.target.checked) { this.setState({tab: 'product-list'}); }}} />
    <label onClick={e => e.stopPropagation() } htmlFor="product-list">Product list</label>
    <input type="radio" name="tab" id="product-codes" checked={this.state.tab == 'product-codes'} className="ml-4 mr-1 nudge-d-1"
           onClick={e => e.stopPropagation() } onChange={e => { if(e.target.checked) {  this.setState({tab: 'product-codes'}); }}} />
    <label onClick={e => e.stopPropagation() } htmlFor="product-codes">Product codes</label>
  </div> */
  /* <div className="flex justify-end items-baseline">
    <span className="mr-2">View:</span>
    <a href="#" className={classNames("border rounded-l-sm px-2 py-1 no-underline hover:no-underline", {
        "bg-white border-order-darker text-black shadow-sm-inner-top hover:text-black": this.state.tab == 'households', 
        "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": this.state.tab != 'households' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'households'}); }}>Households</a>
    <a href="#" className={classNames("border px-2 py-1 no-underline hover:no-underline", {
        "bg-white border-order-darker text-black shadow-sm-inner-top hover:text-black": this.state.tab == 'product-list', 
        "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": this.state.tab != 'product-list' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-list'}); }}>Product list</a>
    <a href="#" className={classNames("border rounded-r-sm px-2 py-1 no-underline hover:no-underline", {
        "bg-white border-order-darker text-black shadow-sm-inner-top hover:text-black": this.state.tab == 'product-codes', 
        "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": this.state.tab != 'product-codes' 
      })} onClick={e => { e.preventDefault(); e.stopPropagation(); this.setState({tab: 'product-codes'}); }}>Product codes</a>
  </div> */
  return (
    <div className="mt-4 flex justify-end items-baseline">
      <div className="flex justify-end rounded-sm items-baseline border border-order-darker p-1 bg-white shadow-sm-inner-top">
        <a href="#" className={classNames("border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
            "bg-white border-none text-black hover:text-black": tab == 'households', 
            "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'households' 
          })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('households'); }}>Households</a>
        <a href="#" className={classNames("ml-1 border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
            "bg-white border-none text-black hover:text-black": tab == 'product-list', 
            "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'product-list' 
          })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-list'); }}>Product list</a>
        <a href="#" className={classNames("ml-1 border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
            "bg-white border-none text-black hover:text-black": tab == 'product-codes', 
            "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'product-codes' 
          })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-codes'); }}>Product codes</a>
      </div>
    </div> 
  )
}

const CurrentOrderStatus = ({order}: Props) => {
  return (
    <span>
      {!order?
        <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />No current order</span>
      : order.isComplete?
        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      }
    </span>
  )
}

const CurrentOrderTotal = ({order}: Props) => {
  if(!order) {
    return <span></span>;
  }

  return (
    <span className="flex justify-end">
      {/* <span>Total:</span> */}
      <span className="w-24 font-bold text-right">
      { order.oldTotalIncVat === null || order.oldTotalIncVat == order.totalIncVat?
        <Money amount={order.totalIncVat} />
      : <span>
          <span className="line-through"><Money amount={order.oldTotalIncVat} /></span> 
          <Money className="text-red font-bold" amount={order.totalIncVat} />
        </span>
      }
      </span>
    </span>
  );
}

const CurrentOrderMessages = ({order, householdOrders}: Props) => {
  if(!order) {
    return <span></span>;
  }

  const allComplete = householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
  const orderMinimumReached = !!order && order.totalIncVat >= 25000
  const allHouseholdsUpToDate = !!order && order.allHouseholdsUpToDate;

  return (
    <div className="mt-4 mx-2 bg-blue-lighter border border-blue-light flex text-black px-2 py-1">
      <Icon type={!!householdOrders.length && allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 fill-current mr-2 nudge-d-2" />
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

interface CurrentOrderButtonsProps {
  order: CollectiveOrder | null
  householdOrders: HouseholdOrder[]
  newOrder: () => void
  deleteOrder: () => void
  abandonOrder: () => void
  placeOrder: () => void
}

const CurrentOrderButtons = ({order, householdOrders, newOrder, deleteOrder, abandonOrder, placeOrder}: CurrentOrderButtonsProps) => {
  const allComplete = householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
  const orderMinimumReached = order && order.totalIncVat >= 25000

  const deleteOrderPossible = !householdOrders.length
  const placeOrderPossible = !!householdOrders.length
  const placeOrderAllowed = allComplete /*&& allPaid*/ && orderMinimumReached
  const abandonOrderPossible = !!householdOrders.length

  return (
    <div className="mt-2 flex flex-wrap content-start items-start">
      {!order?
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); newOrder(); }}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new order</button>
      : [
        deleteOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); deleteOrder() }}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
        ,
        abandonOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); abandonOrder() }}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Abandon order</button>
        ,
        placeOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!placeOrderAllowed} onClick={e => {e.preventDefault(); e.stopPropagation(); placeOrder()}}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
      ]}
  </div>
  )
}