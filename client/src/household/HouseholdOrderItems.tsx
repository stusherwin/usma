import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem as Item, ProductCatalogueEntry } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'

import { OrderItem } from 'order/OrderItem'
import { OrderFooter } from 'order/OrderFooter'

export interface HouseholdOrderItemsProps {
  householdOrder: HouseholdOrder
  products?: ProductCatalogueEntry[]
  readOnly?: boolean
  packing?: boolean
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export class HouseholdOrderItems extends React.Component<HouseholdOrderItemsProps, {}> {
  removeItem = (item: Item) => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productId))
      .then(this.props.reload)
  }

  editQuantity = (item: Item, quantity: number) => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productCode, quantity))
      .then(this.props.reload)
  }

  togglePacked = (item: Item) => {
    if (!this.props.householdOrder)
      return

    this.props.request(ServerApi.command.toggleItemPacked(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productCode))
      .then(this.props.reload)
  }

  render() {
    const householdOrder = this.props.householdOrder
    const items = householdOrder.items.filter(i => !i.adjustment || !i.adjustment.productDiscontinued)
    const discontinuedItems = householdOrder.items.filter(i => i.adjustment && i.adjustment.productDiscontinued)

    return !householdOrder.items.length ?
      <div className="px-2 py-4 text-black">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items {!!this.props.products && !this.props.products.length && ' - the product catalogue is empty'}
      </div>
      : <table className="border-collapse w-full">
        {items.map(item =>
          <OrderItem
            item={item}
            orderAbandoned={householdOrder.isAbandoned}
            packing={this.props.packing}
            editItemQuantity={!this.props.readOnly && householdOrder.isOpen && this.editQuantity || undefined}
            removeItem={!this.props.readOnly && householdOrder.isOpen && this.removeItem || undefined}
            toggleItemPacked={this.props.readOnly && this.togglePacked || undefined}
            {...this.props} />
        )}
        <tr hidden={!discontinuedItems.length}>
          <td colSpan={4} className={classNames("text-red font-bold pb-2 px-2", { "pt-4": !items.length, "pt-8": items.length })}>
            <span className="flex justify-start">
              <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
            </span>
          </td>
        </tr>
        {discontinuedItems.map(item =>
          <OrderItem
            item={item}
            orderAbandoned={householdOrder.isAbandoned}
            {...this.props} />
        )}
        <OrderFooter order={householdOrder} />
      </table>
  }
}