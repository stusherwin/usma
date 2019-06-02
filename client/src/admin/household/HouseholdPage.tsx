import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { Router } from '../../common/Router'
import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'
import { TopNav } from '../TopNav'

export interface HouseholdOrdersPageProps { household: Household
                                          , currentHouseholdOrder: HouseholdOrder | null
                                          , pastHouseholdOrders: PastHouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | null
                                          , payments: HouseholdPayment[]
                                          , products: ProductCatalogueEntry[]
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
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }
  
  render() {
    const currentOrder = this.props.currentHouseholdOrder
    const currentCollectiveOrder = this.props.currentCollectiveOrder
    
    return (
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <TopNav className="text-household-dark hover:text-household-darker" />
        <div className="bg-household-light min-h-screen">
          <div className="p-2 text-black min-h-16">
            <div className="bg-img-household bg-no-repeat w-16 h-16 absolute pin-l ml-2 mt-2"></div>
            <div className="ml-20">
              <h2 className="text-household-darker leading-none mt-2 relative flex">{this.props.household.name}</h2>
              <table className="border-collapse w-full text-household-darker mt-2 mb-2">
                <tbody>
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
                    <td className="text-right"><Money amount={this.props.household.balance} /></td>
                  </tr>
                </tbody>
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
                    <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(currentOrder.orderCreatedDate)}</span><span><Money amount={currentOrder.totalIncVat} /></span></h3>
                    <h3 className="font-normal">{currentOrder.status}</h3>
                  </div>
                </div>
                <CurrentHouseholdOrder householdOrder={currentOrder}
                                       products={this.props.products}
                                       loading={this.props.loading}
                                       reload={this.props.reload}
                                       request={this.props.request} />
              </div>
            )
            : currentCollectiveOrder
            ? (
              <div className="bg-order-dark p-2">
                <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
                  <h2>Current order</h2>
                  <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's an order currently in progress: <strong>{Util.formatDate(currentCollectiveOrder.createdDate)}</strong></p>
                  <button className="mt-2" onClick={_ => this.joinOrder(currentCollectiveOrder.id)}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
                </div>
              </div>
            )
            : (
              <div className="bg-order-dark p-2">
                <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
                  <h2>Current order</h2>
                  <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
                  <button className="mt-2" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
                </div>
              </div>
            )}
            <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                                 request={this.props.request}
                                 reload={this.props.reload} />
            <HouseholdPayments household={this.props.household}
                               payments={this.props.payments}
                               request={this.props.request}
                               reload={this.props.reload} />
          </div>
        </div>
      </div>
    )
  }
}