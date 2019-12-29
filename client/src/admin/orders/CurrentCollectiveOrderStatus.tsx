import * as React from 'react';

import { CollectiveOrder } from '../../util/Types'
import { Icon } from '../../util/Icon'

interface CurrentCollectiveOrderStatusProps {
  order: CollectiveOrder | null
}

export const CurrentCollectiveOrderStatus = ({order}: CurrentCollectiveOrderStatusProps) => {
  return (
    <span>
      {!order?
        <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />No current order</span>
      : order.isComplete?
        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      }
    </span>
  )
}