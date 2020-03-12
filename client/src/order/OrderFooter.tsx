import * as React from 'react';
import * as classNames from 'classnames'

import { OrderAdjustment } from 'util/Types'
import { Money } from 'util/Money'

export interface OrderFooterProps {
  order: {
    totalExcVat: number
    totalIncVat: number
    isAbandoned: boolean
    adjustment: OrderAdjustment | null
  }
}

export const OrderFooter = ({order}: OrderFooterProps) => {
  return (
    <React.Fragment>
      <tr>
        <td></td>
        <td className={classNames('pt-4 align-baseline px-2')} colSpan={4}>
          <div className="flex justify-between">
            <span>VAT:</span>
            <span className={classNames('text-right align-baseline whitespace-no-wrap flex-no-shrink flex-no-grow')}>
              {order.adjustment == null || order.adjustment.oldTotalIncVat - order.adjustment.oldTotalExcVat == order.totalIncVat - order.totalExcVat?
                <Money className={classNames({"line-through text-black": order.isAbandoned})} amount={order.totalIncVat - order.totalExcVat} />
              : <span className="inline-flex flex-col">
                  <Money className="line-through text-black mr-2" amount={order.adjustment.oldTotalIncVat - order.adjustment.oldTotalExcVat} />
                  <Money className="text-red font-bold" amount={order.totalIncVat - order.totalExcVat} />
                </span>
              }
            </span>
          </div>
        </td>
      </tr>
      <tr>
        <td></td>
        <td className={classNames('pt-4 pb-4 px-2 align-baseline font-bold')} colSpan={4}>
          <div className="flex justify-between">
            <span>Total:</span>
            <span className={classNames('text-right align-baseline font-bold whitespace-no-wrap flex-no-shrink flex-no-grow')}>
              {order.adjustment == null || order.adjustment.oldTotalIncVat == order.totalIncVat?
                <Money className={classNames({"line-through text-black": order.isAbandoned})} amount={order.totalIncVat} />
              : <span className="inline-flex flex-col">
                  <Money className="line-through text-black" amount={order.adjustment.oldTotalIncVat} />
                  <Money className="text-red font-bold" amount={order.totalIncVat} />
                </span>
              }
            </span>
          </div>
        </td>
      </tr>
    </React.Fragment>
  )
}
