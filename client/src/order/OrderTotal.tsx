import * as React from 'react';
import * as classNames from 'classnames'

import { Order } from '../util/Types'
import { Money } from '../util/Money'

export interface OrderTotalProps {
  order: Order | null
}

export const OrderTotal = ({order}: OrderTotalProps) => {
  if(!order) {
    return <Money amount={0} />
  }

  return (
    <span className="flex justify-end">
      <span className="w-24 font-bold text-right">
      { order.oldTotalIncVat === null || order.oldTotalIncVat === undefined || order.oldTotalIncVat == order.totalIncVat?
        <Money className={classNames({'line-through text-grey-darker': order.isAbandoned})} amount={order.totalIncVat} />
      : <span>
          <Money className="line-through text-grey-darker" amount={order.oldTotalIncVat} />
          <Money className="text-red" amount={order.totalIncVat} />
        </span>
      }
      </span>
    </span>
  );
}
