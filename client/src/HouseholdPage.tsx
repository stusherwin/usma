import * as React from 'react';

import { Household, HouseholdOrder, CollectiveOrder, HouseholdPayment, Product } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'
import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { HouseholdOrders } from './HouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'

export interface HouseholdOrdersPageProps { household: Household
                                          , householdOrders: HouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | undefined
                                          , payments: HouseholdPayment[]
                                          , products: Product[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, {}> {
  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(id => this.props.reload()
                    .then(_ => Router.navigate(`/households/${householdId}/orders/${id}`)))
  }

  joinOrder = (orderId: number) => {
    const date = new Date()
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/households/${householdId}/orders/${orderId}`))
  }

  render() {
    const currentOrder = this.props.householdOrders.filter(ho => !ho.isOrderPast && !ho.isCancelled)[0]
    const pastOrders = this.props.householdOrders.filter(ho => ho.isOrderPast || ho.isCancelled)
    const currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <div>
        <div className="bg-img-household bg-no-repeat bg-16 pl-16 min-h-16">
          <div><RouterLink path="/households">Households</RouterLink> &gt;</div>
          <h1>{this.props.household.name}</h1>
          <div>Total orders: <Money amount={this.props.household.totalOrders} /></div>
          <div>Total payments: <Money amount={this.props.household.totalPayments} /></div>
          <div>Balance: <Money amount={this.props.household.balance} /></div>
        </div>
        <div>
          {currentOrder
          ? (
            <div>
              <h2>Current order: {Util.formatDate(currentOrder.orderCreatedDate)}</h2>
              <CurrentHouseholdOrder householdOrder={currentOrder}
                                     products={this.props.products}
                                     reload={this.props.reload}
                                     request={this.props.request} />
            </div>
          )
          : currentCollectiveOrder && !currentCollectiveOrder.isCancelled
          ? (
            <div>
              <h2>Current order: {Util.formatDate(currentCollectiveOrder.createdDate)}</h2>
              <p>There's already an order currently in progress.</p>
              <Button action={_ => this.joinOrder(currentCollectiveOrder.id)}>Join it</Button>
            </div>
          )
          : (
            <div>
              <h2>Current order</h2>
              <p>There's no order currently in progress.</p>
              <Button action={this.newOrder}>Start a new one</Button>
            </div>
          )}
          <HouseholdOrders household={this.props.household}
                           householdOrders={this.props.householdOrders}
                           currentCollectiveOrder={this.props.currentCollectiveOrder}
                           request={this.props.request}
                           reload={this.props.reload} />
          <HouseholdPayments household={this.props.household}
                             payments={this.props.payments}
                             request={this.props.request}
                             reload={this.props.reload} />
        </div>
      </div>
    )
  }
}