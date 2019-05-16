import * as React from 'react';

import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { Household, HouseholdOrder, CollectiveOrder, ProductCatalogueEntry } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { ServerApi } from '../ServerApi'
import { CollapsibleWithHeader } from './CollapsibleWithHeader'

export interface CurrentOrderProps { household: Household
                                   , currentOrder: CollectiveOrder | null
                                   , currentHouseholdOrders: HouseholdOrder[]
                                   , currentHouseholdOrder: HouseholdOrder | null
                                   , products: ProductCatalogueEntry[]
                                   , households: Household[]
                                   , loading: boolean
                                   , expanded: boolean
                                   , otherExpanding: boolean
                                   , toggle: () => void
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { addingProduct: boolean
                                   }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  constructor(props: CurrentOrderProps) {
    super(props)

    this.state = { addingProduct: false
                 }
  }

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () => this.setState({ addingProduct: false })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if(!this.state.addingProduct) return Promise.resolve();
    if(!this.props.currentHouseholdOrder) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.currentOrder)
      return

    const orderId = this.props.currentOrder.id
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }

  render() {
    let householdOrder = this.props.currentHouseholdOrder
    let order = this.props.currentOrder
 
    return (
      <CollapsibleWithHeader className="min-h-20"
                             headerClassName="bg-order-dark min-h-20"
                             headerImageClassName="bg-img-order"
                             headerText="Current order"
                             headerContent={() => (
                              <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span>
                                <span>
                                  {householdOrder 
                                  ? householdOrder.oldTotalIncVat !== null && householdOrder.oldTotalIncVat != householdOrder.totalIncVat && !householdOrder.isAbandoned
                                    ? <span>
                                        <span className="line-through"><Money amount={householdOrder.oldTotalIncVat} /></span> 
                                        <Money className="text-red font-bold" amount={householdOrder.totalIncVat} />
                                      </span>
                                    : <Money amount={!householdOrder.isAbandoned? householdOrder.totalIncVat : 0} />
                                  : <Money amount={0} />
                                  }
                                </span>
                              </h3>
                             )}
                             onCollapse={this.cancelAdd}
                             {...this.props}>
        { householdOrder && order
          ? (
            <CurrentHouseholdOrder currentOrder={order}
                                   currentHouseholdOrder={householdOrder}
                                   currentHouseholdOrders={this.props.currentHouseholdOrders}
                                   products={this.props.products}
                                   households={this.props.households}
                                   loading={this.props.loading}
                                   reload={this.props.reload}
                                   request={this.props.request}
                                   addingProduct={this.state.addingProduct}
                                   startAdd={this.startAdd}
                                   cancelAdd={this.cancelAdd}
                                   confirmAdd={this.confirmAdd} />
          )
          : (order
          ? (
            <div className="shadow-inner-top bg-white px-2 py-4 text-grey-darker">
              <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><strong>{order.createdBy == this.props.household.id ? 'You' : order.createdByName}</strong> started an order on <strong>{Util.formatDate(order.createdDate)}</strong></p  >
              <button className="mt-4" onClick={_ => this.joinOrder()}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
            </div>
          )
          : (
            <div className="shadow-inner-top bg-white px-2 py-4 text-grey-darker">
              <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
              <button className="mt-4" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
            </div>
         ))
        }
     </CollapsibleWithHeader>
   )
  }
}