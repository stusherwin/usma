import * as React from 'react';

import { Order } from 'util/Types'
import { Icon } from 'util/Icon'

export const OrderStatus = ({order}: {order: Order | undefined}) => {
  return (
    <span>
      {!order?
        <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      : order.isReconciled?
        <span><Icon type="clipboard-check" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Reconciled</span>
      : order.isAbandoned || order.orderIsAbandoned?
        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Abandoned</span>
      : order.orderIsPlaced?
        <span><Icon type="cart" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Placed</span>
      : order.isComplete?
        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      }
    </span>
  )
}