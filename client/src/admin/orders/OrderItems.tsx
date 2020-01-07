import * as React from 'react';
import * as classNames from 'classnames'

import { Order } from '../../util/Types'
import { Money } from '../../util/Money'
import { Icon } from '../../util/Icon'

import { OrderItem } from '../../order/OrderItem'

export interface OrderItemsProps { order: Order
                                 }

export const OrderItems = ({order}: OrderItemsProps) =>
  !order.items.length?
    <div className="px-2 py-4 text-grey-darker">
      <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
    </div>
  : <table className="border-collapse w-full">
      {order.items.map((item, index) => 
        <OrderItem item={item} index={index} />
      )}
      <tr>
        <td></td>
        <td className={classNames('pt-4 align-baseline px-2')} colSpan={4}>
          <div className="flex justify-between">
            <span>VAT:</span>
            <span className={classNames('text-right')}>
              <span><Money amount={order.totalIncVat - order.totalExcVat} /></span>
            </span>
          </div>
        </td>
      </tr>
      <tr>
        <td></td>
        <td className={classNames('pt-4 align-baseline px-2 pb-4')} colSpan={4}>
          <div className="flex justify-between font-bold">
            <span>Total:</span>
            <span className="text-right">
              <Money className="flex-no-shrink flex-no-grow text-right" amount={order.totalIncVat} />
            </span>
          </div>
        </td>
      </tr>
    </table>