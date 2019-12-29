import * as React from 'react';

import { CollectiveOrder, HouseholdOrder } from '../../util/Types'
import { Icon } from '../../util/Icon'

export interface CurrentCollectiveOrderMessagesProps {
  order: CollectiveOrder | null
}

export const CurrentCollectiveOrderMessages = ({order}: CurrentCollectiveOrderMessagesProps) => {
  if(!order) {
    return <span></span>;
  }

  const allComplete = order.householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
  const orderMinimumReached = order.totalIncVat >= 25000
  const allHouseholdsUpToDate = order.allHouseholdsUpToDate;

  return (
    <div className="mt-4 mx-2 bg-blue-lighter border border-blue-light flex text-black px-2 py-1">
      <Icon type={!!order.householdOrders.length && allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 fill-current mr-2 nudge-d-2" />
      { !order.householdOrders.length?
        <span>Waiting for households to join</span>
      : !allHouseholdsUpToDate?
        <span>Waiting for all households to accept latest catalogue updates</span>
      : !orderMinimumReached?
        <span>Waiting for &pound;250.00 order minimum</span>
      : !allComplete?
        <span>Waiting for all orders to be completed</span>
        // : !allPaid?
        //   <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
      : <span>Order can now be placed</span>
      }
    </div>
  )
}