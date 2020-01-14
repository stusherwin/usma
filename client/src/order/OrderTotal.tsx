import * as React from 'react';
import * as classNames from 'classnames'

import { Order } from 'util/Types'
import { Money } from 'util/Money'

export interface OrderTotalProps {
  order: Order | undefined
}

export const OrderTotal = ({order}: OrderTotalProps) => {
  if(!order) {
    return <Money amount={0} />
  }

  return (
    <span className="flex justify-end">
      <span className="font-bold text-right">
      { order.adjustment == null || order.adjustment.oldTotalIncVat == order.totalIncVat?
        <Money className={classNames({'line-through text-grey-darker': order.isAbandoned})} amount={order.totalIncVat} />
      : <span>
          <Money className="line-through text-grey-darker mr-2" amount={order.adjustment.oldTotalIncVat} />
          <Money className="text-red" amount={order.totalIncVat} />
        </span>
      }
      </span>
    </span>
  );
}
