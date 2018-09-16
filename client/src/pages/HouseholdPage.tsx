import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, CollectiveOrder, HouseholdPayment, Product } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Icon } from '../Icon'
import { Money } from '../Money'
import { Router } from '../Router'
import { CurrentHouseholdOrder } from '../CurrentHouseholdOrder'
import { HouseholdOrders } from '../HouseholdOrders'
import { HouseholdPayments } from '../HouseholdPayments'
import { TopNav } from '../TopNav'

export interface HouseholdOrdersPageProps { household: Household
                                          , householdOrders: HouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | undefined
                                          , payments: HouseholdPayment[]
                                          , products: Product[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          , loading: boolean
                                          , error: ApiError | null
                                          }

export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, {}> {
  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = (orderId: number) => {
    const date = new Date()
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }
  
  render() {
    const currentOrder = this.props.householdOrders.filter(ho => !ho.isOrderPast)[0]
    const pastOrders = this.props.householdOrders.filter(ho => ho.isOrderPast)
    const currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-household-light p-2">
          <TopNav className="text-household-dark hover:text-household-darker" />
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="leading-none mb-2 -mt-1 text-household-darker">{this.props.household.name}{!!this.props.loading && <Icon type="refresh" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <table className="border-collapse w-full text-household-darker">
              <tr>
                <td>Total orders:</td>
                <td className="text-right"><Money amount={this.props.household.totalOrders} /></td>
              </tr>
              <tr>
                <td>Total payments:</td>
                <td className="text-right"><Money amount={this.props.household.totalPayments} /></td>
              </tr>
              <tr>
                <td>Balance:</td>
                <td className={classNames('text-right', {'text-red-dark': this.props.household.balance < 0})}><Money amount={this.props.household.balance} /></td>
              </tr>
            </table>
          </div>
        </div>
        <div>
          {currentOrder
          ? (
            <div>
              <div className="bg-order-dark p-2">
                <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative">
                  <h2>Current order</h2>
                  <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(currentOrder.orderCreatedDate)}</span><span><Money amount={currentOrder.total} /></span></h3>
                  <h3 className="font-normal">{currentOrder.status}</h3>
                </div>
              </div>
              <CurrentHouseholdOrder householdOrder={currentOrder}
                                     products={this.props.products}
                                     reload={this.props.reload}
                                     request={this.props.request} />
            </div>
          )
          : currentCollectiveOrder && !currentCollectiveOrder.isCancelled
          ? (
            <div className="bg-order-dark p-2">
              <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
                <h2>Current order</h2>
                <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's an order currently in progress: <strong>{Util.formatDate(currentCollectiveOrder.createdDate)}</strong></p>
                <Button className="mt-2" action={_ => this.joinOrder(currentCollectiveOrder.id)}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</Button>
              </div>
            </div>
          )
          : (
            <div className="bg-order-dark p-2">
              <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
                <h2>Current order</h2>
                <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
                <Button className="mt-2" action={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</Button>
              </div>
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