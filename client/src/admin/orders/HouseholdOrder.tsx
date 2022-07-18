import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder as Order, ProductCatalogueEntry } from 'util/Types'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { AcceptUpdates } from 'household/AcceptUpdates'
import { AddProduct } from 'household/AddProduct'
import { HouseholdOrderItems } from 'household/HouseholdOrderItems'
import { HouseholdOrderButtons } from 'household/HouseholdOrderButtons'

import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'

export interface HouseholdOrderProps {
  order: CollectiveOrder
  householdOrder: Order
  products: ProductCatalogueEntry[]
  categories: string[]
  brands: string[]
  collapsibleState: CollapsibleState
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export interface HouseholdOrderState {
  addingProduct: boolean
}

export class HouseholdOrder extends React.Component<HouseholdOrderProps, HouseholdOrderState> {
  constructor(props: HouseholdOrderProps) {
    super(props)

    this.state = {
      addingProduct: false
    }
  }

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () => this.setState({ addingProduct: false })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if (!this.state.addingProduct) return Promise.resolve();
    if (!this.props.order) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.order.id, this.props.householdOrder.householdId, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.householdOrder.householdId
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  abandonOrder = () => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.abandonHouseholdOrder(this.props.order.id, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  completeOrder = () => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.completeHouseholdOrder(this.props.order.id, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  reopenOrder = () => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.reopenHouseholdOrder(this.props.order.id, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  unusedProducts = () => {
    if (!this.props.householdOrder)
      return this.props.products

    const items = this.props.householdOrder.items
    return this.props.products.filter(p => !items.find(i => i.productCode == p.code))
  }

  acceptUpdates = () => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.acceptCatalogueUpdates(this.props.order.id, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  render() {
    let unusedProducts = this.unusedProducts()

    return (
      <div key={this.props.householdOrder.householdId}>
        <Collapsible
          collapsibleKey={this.props.householdOrder.householdId}
          collapsibleState={this.props.collapsibleState}
          header={
            <div className="p-2 pt-4 bg-household-lighter min-h-24">
              <svg className="w-16 h-16 absolute">
                <use xlinkHref="#icon-household" />
              </svg>
              <div className="flex items-baseline justify-between ml-20">
                <div>
                  <h3 className={classNames("leading-none", { "line-through": this.props.householdOrder.isAbandoned })}>
                    STU {this.props.householdOrder.householdName}
                  </h3>
                  <h4 className="mt-4 mb-4">
                    <OrderStatus order={this.props.householdOrder} />
                  </h4>
                </div>
                <h4 className="ml-2">
                  <OrderTotal order={this.props.householdOrder} />
                </h4>
              </div>
            </div>
          }
          expandedHeader={!this.state.addingProduct &&
            <HouseholdOrderButtons
              className="p-2 bg-household-lighter -mt-4"
              unusedProducts={unusedProducts}
              currentHouseholdOrder={this.props.householdOrder}
              collectiveOrder={this.props.order}
              newOrder={this.newOrder}
              reopenOrder={this.reopenOrder}
              abandonOrder={this.abandonOrder}
              completeOrder={this.completeOrder}
              startAdd={this.startAdd} />
            || undefined}>
          <div className="bg-white border-t border-household-light shadow-inner-top">
            { this.state.addingProduct ?
              <AddProduct
                products={unusedProducts}
                cancelAdd={this.cancelAdd}
                confirmAdd={this.confirmAdd}
                {...this.props} />
            : <div>
                <AcceptUpdates
                  householdOrder={this.props.householdOrder}
                  acceptUpdates={this.acceptUpdates} />
                <HouseholdOrderItems
                  householdOrder={this.props.householdOrder}
                  readOnly={true}
                  packing={true}
                  {...this.props} />
              </div>
            }
          </div>
        </Collapsible>
      </div>
    )
  }
}