import * as React from 'react';

import { HouseholdOrder } from 'util/Types'
import { Icon } from 'util/Icon'

export interface AcceptUpdatesProps { householdOrder: HouseholdOrder
                                      acceptUpdates: () => void
                                    }

export const AcceptUpdates = ({householdOrder, acceptUpdates}: AcceptUpdatesProps) => {
  if(!householdOrder.adjustment || householdOrder.orderIsPlaced || householdOrder.orderIsAbandoned)
    return null

  return (
    <div className="bg-red-lighter p-2 py-4 mb-4 shadow-inner-top">
      <div className="flex"><Icon type="alert" className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-1" />The product catalogue was updated and your order has been affected. Please review and accept the changes before continuing.</div>
      <div className="flex justify-end mt-2"><button className="whitespace-no-wrap flex-no-shrink flex-no-grow" onClick={e => {e.stopPropagation(); acceptUpdates()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Accept changes</button></div>
    </div>
  )
}