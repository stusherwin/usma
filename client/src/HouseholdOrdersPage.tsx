import * as React from 'react';

import { Household, HouseholdOrder, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'
import { Router } from './Router'

export interface HouseholdOrdersPageProps { household: Household
                                          , householdOrders: HouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | undefined
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => void
                                          }

export class HouseholdOrdersPage extends React.Component<HouseholdOrdersPageProps, {}> {
  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(id => {
        this.props.reload()
        return id
      })
      .then(id => Router.navigate(`/households/${householdId}/orders/${id}`))
  }

  joinOrder = (orderId: number) => {
    const date = new Date()
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.addHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/households/${householdId}/orders/${orderId}`))
  }

  render() {
    const currentOrder = this.props.householdOrders.filter(ho => !ho.isOrderPast && !ho.isCancelled)[0]
    const pastOrders = this.props.householdOrders.filter(ho => ho.isOrderPast || ho.isCancelled)
    const currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <div>
        <div><RouterLink path="/households">Households</RouterLink> &gt;</div>
        <h1>{this.props.household.name}</h1>
        <div>
          <div>Orders</div>
          <RouterLink path={`/households/${this.props.household.id}/payments`}>Payments</RouterLink>

          <h2>Current order</h2>
          {currentOrder
          ? (
            <div>
              <div>
                <span>{ Util.formatDate(currentOrder.orderCreatedDate) }</span>
                <Money amount={currentOrder.total} />
                <RouterLink path={`/households/${currentOrder.householdId}/orders/${currentOrder.orderId}`}>View</RouterLink>
              </div>
            </div>
          )
          : currentCollectiveOrder && !currentCollectiveOrder.isCancelled
            ? <Link action={_ => this.joinOrder(currentCollectiveOrder.id)}>Join existing order</Link>
            : <Link action={this.newOrder}>Start new order</Link>
          }
          <h2>Past orders</h2>
          {!pastOrders.length ? <div>No past orders</div> : (
            <div>
              { pastOrders.map(ho => (
                <div key={ho.orderId}>
                  <span>{ Util.formatDate(ho.orderCreatedDate) }</span>
                  <span>{ho.isCancelled && 'cancelled'}</span>
                  <Money amount={ho.total} />
                  <RouterLink path={`/households/${ho.householdId}/orders/${ho.orderId}`}>View</RouterLink>
                </div>
              )) }
            </div>
          )}
        </div>
      </div>
    )
  }
}