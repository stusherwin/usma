import * as React from 'react';

import { HouseholdOrder, CollectiveOrder } from '../util/Types'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'

interface CollectiveOrderMessagesProps { householdOrder: HouseholdOrder | null
                                         collectiveOrder: CollectiveOrder
                                         acceptUpdates: () => void
                                       }

export const CollectiveOrderMessages = ({householdOrder, collectiveOrder, acceptUpdates}: CollectiveOrderMessagesProps) => {
  if(!householdOrder)
    return null

  const allComplete = !!collectiveOrder && collectiveOrder.householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
  const allHouseholdsUpToDate = !!collectiveOrder && collectiveOrder.allHouseholdsUpToDate;
  const orderMinimumReached = !!collectiveOrder && collectiveOrder.totalIncVat >= 25000

  if(!!householdOrder.oldTotalExcVat && householdOrder.oldTotalIncVat != householdOrder.totalIncVat)
    return (
      <div className="flex bg-red-lighter p-2 mb-4">
        <Icon type="alert" className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-2" />The product catalogue was updated and your order has been affected. Please review and accept the changes before continuing.
        <div className="flex justify-end mt-2"><button onClick={e => {e.stopPropagation(); acceptUpdates()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Accept changes</button></div>
      </div>
    )

  if(!householdOrder.isComplete)
    return null

  return (
    <div className="flex px-2 py-4 text-black">
      <Icon type={allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 mr-2 fill-current" />
      { !allHouseholdsUpToDate?
        <span>Waiting for all households to accept latest catalogue updates</span>
      : !orderMinimumReached?
        <span>Waiting for minimum order to be reached. Current total is <Money amount={!!collectiveOrder && collectiveOrder.totalIncVat || 0} /> of &pound;250.00</span>
      : !allComplete?
        <span>Waiting for all orders to be completed</span>
      // : !allPaid?
      //   <span>, waiting for everyone to pay up</span>
      : <span>Waiting for admin to place order</span>
      }
    </div>
  )
}