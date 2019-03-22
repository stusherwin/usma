import * as React from 'react';
import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { ServerApi, ApiError } from '../ServerApi'

export interface CurrentOrderProps { household: Household
                                   , currentOrder: HouseholdOrder | null
                                   , currentCollectiveOrder: CollectiveOrder | null
                                   , products: ProductCatalogueEntry[]
                                   , loading: boolean
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export class CurrentOrder extends React.Component<CurrentOrderProps, {}> {
  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.currentCollectiveOrder)
      return

    const orderId = this.props.currentCollectiveOrder.id
    const date = new Date()
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }

  render() {
    let currentOrder = this.props.currentOrder
    let currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      currentOrder
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
      : (currentCollectiveOrder
      ? (
        <div className="bg-order-dark p-2">
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
            <h2>Current order</h2>
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's an order currently in progress: <strong>{Util.formatDate(currentCollectiveOrder.createdDate)}</strong></p>
            <button className="mt-2" onClick={_ => this.joinOrder()}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
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
      ))
    )
  }
}