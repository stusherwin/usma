import * as React from 'react';

import { Order } from 'util/Types'
import { Icon } from 'util/Icon'

export const OrderStatus = ({order}: {order: Order | undefined}) => {
  return (
    <span>
      {!order?
        <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />None</span>
      : order.isComplete?
        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
      : order.isAbandoned?
        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Abandoned</span>
      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      }
    </span>
  )
}