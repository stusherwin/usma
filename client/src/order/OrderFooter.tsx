import * as React from 'react';
import * as classNames from 'classnames'

import { Order } from '../util/Types'
import { Money } from '../util/Money'

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
              {order.oldTotalIncVat === null || order.oldTotalIncVat === undefined || order.oldTotalExcVat === null || order.oldTotalExcVat === undefined || order.oldTotalIncVat - order.oldTotalExcVat == order.totalIncVat - order.totalExcVat?
                <span className={classNames({"line-through text-grey-dark": order.isAbandoned})}><Money amount={order.totalIncVat - order.totalExcVat} /></span>
              : <span>
                  <span className="line-through text-grey-dark"><Money amount={order.oldTotalIncVat - order.oldTotalExcVat} /></span> 
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
              {order.oldTotalIncVat === null || order.oldTotalIncVat === undefined || order.oldTotalIncVat == order.totalIncVat?
                <span className={classNames({"line-through text-grey-dark": order.isAbandoned})}><Money amount={order.totalIncVat} /></span>
              : <span>
                  <span className="line-through text-grey-dark"><Money amount={order.oldTotalIncVat} /></span> 
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
