import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Icon } from '../../util/Icon'
import { Money } from '../../util/Money'
import { Router } from '../../util/Router'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { HouseholdOrders } from './CurrentHouseholdOrders'
import { CurrentCollectiveOrderItems } from './CurrentCollectiveOrderItems'
import { CurrentCollectiveOrderProductCodes } from './CurrentCollectiveOrderProductCodes'
import { CollectiveOrderTabs } from './CollectiveOrderTabs'

export interface CurrentCollectiveOrderProps { currentOrder: CollectiveOrder | null
                                   , currentHouseholdOrders: HouseholdOrder[]
                                   , households: Household[]
                                   , collapsibleKey: string
                                   , collapsibleState: CollapsibleState
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentCollectiveOrderState { addingHousehold: Household | null
                                   , tab: 'households' | 'product-list' | 'product-codes'
                                   }

export class CurrentCollectiveOrder extends React.Component<CurrentCollectiveOrderProps, CurrentCollectiveOrderState> {
  constructor(props: CurrentCollectiveOrderProps) {
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
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={
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
                       {!!order
                       ? <CollectiveOrderTabs tab={this.state.tab} setTab={tab => this.setState({tab})} />
                       : <span></span>
                       }
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
                                 {...this.props} />
              : this.state.tab == 'product-list'?
                <CurrentCollectiveOrderItems currentOrder={order} />
              : <CurrentCollectiveOrderProductCodes currentOrder={order} />
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