import * as React from 'react';

import { Household, HouseholdOrder, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface HouseholdOrdersPageProps { household: Household
                                          , householdOrders: HouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | undefined
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , navigate: (location: string) => void
                                          , reload: () => void
                                          }

export class HouseholdOrdersPage extends React.Component<HouseholdOrdersPageProps, {}> {
  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(id => this.props.navigate(`/orders/${id}/households/${householdId}`))
  }

  joinOrder = (orderId: number) => {
    const date = new Date()
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.addHouseholdOrder(orderId, householdId))
      .then(id => this.props.navigate(`/orders/${orderId}/households/${householdId}`))
  }

  render() {
    const currentOrder = this.props.householdOrders.filter(ho => !ho.isOrderPast && !ho.isCancelled)[0]
    const pastOrders = this.props.householdOrders.filter(ho => ho.isOrderPast || ho.isCancelled)
    const currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <div>
        <h1>Orders</h1>
        <h2>Current order</h2>
        {currentOrder
        ? (
          <div>
            <div>
              <span>{ Util.formatDate(currentOrder.orderCreatedDate) }</span>
              <Money amount={currentOrder.total} />
              <Link action={_ => this.props.navigate(`/orders/${currentOrder.orderId}/households/${currentOrder.householdId}`)}>View</Link>
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
                <Link action={_ => this.props.navigate(`/orders/${ho.orderId}/households/${ho.householdId}`)}>View</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}