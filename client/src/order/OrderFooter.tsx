import * as React from 'react';
import * as classNames from 'classnames'

import { Order } from 'util/Types'
import { Money } from 'util/Money'

export interface OrderFooterProps {
  order: Order
}

export const OrderFooter = ({order}: OrderFooterProps) => {
  return (
    <React.Fragment>
      <tr>
        <td></td>
        <td className={classNames('pt-8 align-baseline px-2')} colSpan={4}>
          <div className="flex justify-between">
            <span>VAT:</span>
            <span className={classNames('text-right align-baseline whitespace-no-wrap flex-no-shrink flex-no-grow')}>
              {order.adjustment == null || order.adjustment.oldTotalIncVat - order.adjustment.oldTotalExcVat == order.totalIncVat - order.totalExcVat?
                <Money className={classNames({"line-through text-grey-darker": order.isAbandoned})} amount={order.totalIncVat - order.totalExcVat} />
              : <span>
                  <Money className="line-through text-grey-darker mr-2" amount={order.adjustment.oldTotalIncVat - order.adjustment.oldTotalExcVat} />
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
                <Money className={classNames({"line-through text-grey-darker": order.isAbandoned})} amount={order.totalIncVat} />
              : <span>
                  <Money className="line-through text-grey-darker mr-2" amount={order.adjustment.oldTotalIncVat} />
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
