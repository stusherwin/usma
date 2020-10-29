import * as React from 'react';

import { Order } from 'util/Types'
import { Icon } from 'util/Icon'

import { OrderItem } from './OrderItem'
import { OrderFooter } from './OrderFooter'

export interface OrderItemsProps {
  order: Order
  showProductImage: (productCode: string) => void
}

export const OrderItems = ({ order }: OrderItemsProps) =>
  !order.items.length ?
    <div className="px-2 py-4 text-black">
      <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
    </div>
    : <table className="border-collapse w-full">
      {order.items.map(item =>
        <OrderItem
          item={item}
          orderAbandoned={order.isAbandoned}
          {...this.props} />
      )}
      <OrderFooter order={order} />
    </table>