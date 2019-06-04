import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Router } from '../../common/Router'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { HouseholdOrders } from './HouseholdOrders'
import { TopNav } from '../TopNav'
import { Header } from '../../household/Header'
import { Loading } from '../../household/Loading'

export interface OrdersPageProps { currentOrder: CollectiveOrder | null
                                 , currentHouseholdOrders: HouseholdOrder[]
                                 , households: Household[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , reload: () => Promise<void>
                                 , loading: boolean
                                 , error: ApiError | null
                                 }
type Section = 'order'
export interface OrdersPageState { expanded: Section | null
                                 , addingHousehold: Household | null
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, OrdersPageState> {  
  constructor(props: OrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'order', 
      addingHousehold: null
    }
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  newOrder = () => {
    // TODO: Have some way of choosing the household to create order
    this.props.request(ServerApi.command.createOrder(this.props.households[0].id))
      .then(this.props.reload)
  }

  startAddHousehold = (h: Household) => this.setState({ addingHousehold: h })

  addingHouseholdChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingHousehold: this.props.households.find(h => '' + h.id == event.target.value) || null })

  cancelAddHousehold = () =>
    this.setState({ addingHousehold: null
                  })

  confirmAddHousehold = () => {
    if(!this.props.currentOrder) return
    if(!this.state.addingHousehold) return

    this.props.request(ServerApi.command.createHouseholdOrder(this.props.currentOrder.id, this.state.addingHousehold.id))
      .then(this.props.reload)
      .then(_ => this.setState({ addingHousehold: null
                               }))
  }

  deleteOrder = () => {
    if(!this.props.currentOrder) return

    this.props.request(ServerApi.command.deleteOrder(this.props.currentOrder.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/admin/orders`))
  }

  placeOrder = () => {
    Router.navigate('/admin/orders/place')
  }

  abandonOrder = () => {
    if(!this.props.currentOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.currentOrder.id))
      .then(this.props.reload)
  }

  render() {
    const currentOrder = this.props.currentOrder
    const householdOrders = this.props.currentHouseholdOrders

    const unusedHouseholds = this.props.households.filter(h => !householdOrders.find(oh => oh.householdId == h.id))

    return (
      <div className="bg-order-dark min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <TopNav />
        <Header className="bg-order-dark min-h-20"
                imageClassName="bg-img-order"
                headingText="Current order"
                content={() => (
                 <div>
                   <h3 className="flex justify-between ml-20 mt-4 mb-4">
                     <span>Total:</span>
                     {this.renderTotal()}
                   </h3>
                   {this.renderMessages()}
                   {this.renderButtons(unusedHouseholds)}
                 </div>
                )} />
        <div className="bg-white shadow-inner-top">
          {!currentOrder?
            <div className="px-2 py-4">
              <div className="my-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order currently in progress</div>
              <div className="flex justify-start">
                <button onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New order</button>
              </div>
            </div>
          : this.state.addingHousehold?
            <div className="bg-household-lightest shadow-inner-top px-2 py-4">
              <h3 className="mb-4">Add household</h3>
              <select className="mb-4 w-full" value={this.state.addingHousehold.id} onChange={this.addingHouseholdChanged}>
                {unusedHouseholds.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
              </select>
              <div className="flex justify-end">
                <button className="ml-2" onClick={this.confirmAddHousehold}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button>
                <button className="ml-2" onClick={this.cancelAddHousehold}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
              </div>
            </div>
          : !this.props.currentHouseholdOrders.length?
            <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order yet
            </div>
          : <HouseholdOrders order={currentOrder}
                             householdOrders={this.props.currentHouseholdOrders}
                             households={this.props.households}
                             addingHousehold={this.state.addingHousehold}
                             reload={this.props.reload}
                             request={this.props.request} />
          }
        </div>
        <Loading loading={this.props.loading}></Loading>
      </div>
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

    return (
      <div>
        {!householdOrders.length?
            <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for households to join</span>
          : !orderMinimumReached?
            <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for &pound;250.00 order minimum</span>
          : !allComplete?
            <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for all orders to be completed</span>
          // : !allPaid?
          //   <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
          : <span className="text-green"><Icon type="ok" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Good to go</span>
        }
      </div>
    )
  }

  renderButtons = (unusedHouseholds: Household[]) => {
    const order = this.props.currentOrder
    const householdOrders = this.props.currentHouseholdOrders

    const allComplete = householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const orderMinimumReached = order && order.totalIncVat >= 25000

    const deleteOrderPossible = !householdOrders.length
    const addHouseholdPossible = !!unusedHouseholds.length
    const placeOrderPossible = !!householdOrders.length
    const placeOrderAllowed = allComplete /*&& allPaid*/ && orderMinimumReached
    const abandonOrderPossible = !!householdOrders.length

    return (
      <div className="flex flex-wrap content-start items-start">
        {deleteOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!!this.state.addingHousehold} onClick={e => {e.preventDefault(); e.stopPropagation(); this.deleteOrder() }}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
        }
        {abandonOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!!this.state.addingHousehold} onClick={e => {e.preventDefault(); e.stopPropagation(); this.abandonOrder() }}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Abandon order</button>
        }
        {placeOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!!this.state.addingHousehold || !placeOrderAllowed} onClick={e => {e.preventDefault(); e.stopPropagation(); this.placeOrder()}}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
        }
        {addHouseholdPossible &&
          <button className="flex-no-grow flex-no-shrink ml-auto mt-2" disabled={!!this.state.addingHousehold} onClick={e => {e.preventDefault(); e.stopPropagation(); this.startAddHousehold(unusedHouseholds[0])}}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add a household</button>
        }
    </div>
    )
  }
}