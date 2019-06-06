import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Router } from '../../common/Router'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { HouseholdOrders } from './HouseholdOrders'
import { TopNav } from '../TopNav'
import { Collapsible, Header } from '../../household/CollapsibleWithHeader'
import { Loading } from '../../household/Loading'

export interface OrdersPageProps { currentOrder: CollectiveOrder | null
                                 , currentHouseholdOrders: HouseholdOrder[]
                                 , pastOrders: PastCollectiveOrder[]
                                 , households: Household[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , reload: () => Promise<void>
                                 , loading: boolean
                                 , error: ApiError | null
                                 }
type Section = 'order' | 'past-orders'
export interface OrdersPageState { expanded: Section | null }

export class OrdersPage extends React.Component<OrdersPageProps, OrdersPageState> {  
  constructor(props: OrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'order', 
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
    const pastOrders = this.props.pastOrders

    return (
      <div className="bg-order-dark min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <TopNav />
        <Collapsible className="min-h-20"
                     expanded={this.state.expanded == 'order'}
                     otherExpanding={!!this.state.expanded && this.state.expanded != 'order'}
                     toggle={this.toggle('order')}
                     header={() => 
          <Header headerClassName="bg-order-dark min-h-20"
                  headerImageClassName="bg-img-order"
                  headerText="Current order"
                  headerContent={() => (
                   <div>
                     <h3 className="flex justify-between ml-20 mt-4 mb-4">
                       <span>Total:</span>
                       {this.renderTotal()}
                     </h3>
                     {this.renderMessages()}
                     {this.renderButtons()}
                   </div>
                  )} /> }>
          <div className="bg-household-lighter shadow-inner-top">
            {!currentOrder?
              <div className="px-2 py-4">
                <div className="my-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order currently in progress</div>
                <div className="flex justify-start">
                  <button onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New order</button>
                </div>
              </div>
            : !this.props.currentHouseholdOrders.length?
              <div className="px-2 py-4 text-grey-darker">
                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order yet
              </div>
            : <HouseholdOrders order={currentOrder}
                               householdOrders={this.props.currentHouseholdOrders}
                               households={this.props.households}
                               reload={this.props.reload}
                               request={this.props.request} />
            }
          </div>
        </Collapsible>
        <Collapsible className="min-h-20"
                     expanded={this.state.expanded == 'past-orders'}
                     otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders'}
                     toggle={this.toggle('past-orders')}
                     header={() => 
          <Header headerClassName="bg-past-order-lighter min-h-20"
                  headerImageClassName="bg-img-order"
                  headerText="Past orders" /> }>
          <div className="bg-white shadow-inner-top">
            {!pastOrders.length
            ? <div className="px-2 py-4 text-grey-darker">
                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
              </div> 
            : (
              <table className="border-collapse w-full">
                <tbody>
                  { pastOrders.map((o, i) => (
                    <tr key={o.id}>
                      <td className={classNames("pt-4 pl-2 pr-2", {"pb-4": i == pastOrders.length -1})}><RouterLink path={`/admin/orders/${o.id}`}>{ Util.formatDate(o.createdDate)}</RouterLink></td>
                      <td className={classNames("pt-4 pr-2", {"pb-4": i == pastOrders.length -1})}>{o.isAbandoned && 'Abandoned'}</td>
                      <td className={classNames("pt-4 pr-2 text-right", {"pb-4": i == pastOrders.length -1, "line-through text-grey-dark": o.isAbandoned})}><Money amount={o.totalIncVat} /></td>
                    </tr>
                  )) }
                </tbody>
              </table>
            )}
          </div>
        </Collapsible>
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

  renderButtons = () => {
    const order = this.props.currentOrder
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
    </div>
    )
  }
}