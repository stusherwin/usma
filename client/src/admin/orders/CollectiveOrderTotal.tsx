import * as React from 'react';

import { CollectiveOrder } from '../../util/Types'
import { Money } from '../../util/Money'

export interface CollectiveOrderMessagesProps {
  order: CollectiveOrder | null
}

export const CollectiveOrderTotal = ({order}: CollectiveOrderMessagesProps) => {
  if(!order) {
    return <span></span>;
  }

  return (
    <span className="flex justify-end">
      <span className="w-24 font-bold text-right">
      { order.oldTotalIncVat === null || order.oldTotalIncVat === undefined || order.oldTotalIncVat == order.totalIncVat?
        <Money amount={order.totalIncVat} />
      : <span>
          <span className="line-through"><Money amount={order.oldTotalIncVat} /></span> 
          <Money className="text-red font-bold" amount={order.totalIncVat} />
        </span>
      }
      </span>
    </span>
  );
}
