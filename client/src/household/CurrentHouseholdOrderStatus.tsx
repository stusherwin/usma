import * as React from 'react';

import { HouseholdOrder } from '../util/Types'
import { Icon } from '../util/Icon'

export const CurrentHouseholdOrderStatus = ({currentHouseholdOrder}: {currentHouseholdOrder: HouseholdOrder | null}) => {
  return (
    <span>
      {!currentHouseholdOrder?
        <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Available</span>
      : currentHouseholdOrder.isComplete?
        <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
      : currentHouseholdOrder.isAbandoned?
        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Abandoned</span>
      : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Open</span>
      }
    </span>
  )
}