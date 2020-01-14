import * as React from 'react';

import { CollectiveOrder, HouseholdOrder } from 'util/Types'
import { Icon } from 'util/Icon'

interface CollectiveOrderButtonsProps {
  order: CollectiveOrder | undefined
  newOrder: () => void
  deleteOrder: () => void
  abandonOrder: () => void
  placeOrder: () => void
}

export const CollectiveOrderButtons = ({order, newOrder, deleteOrder, abandonOrder, placeOrder}: CollectiveOrderButtonsProps) => {
  const allComplete = !!order && order.householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
  const orderMinimumReached = !!order && order.totalIncVat >= 25000

  const deleteOrderPossible = !!order && !order.householdOrders.length
  const placeOrderPossible = !!order && !order.isAbandoned && !!order.householdOrders.length
  const placeOrderAllowed = !!order && !order.isAbandoned && allComplete /*&& allPaid*/ && orderMinimumReached
  const abandonOrderPossible = !!order && !order.isAbandoned && !!order.householdOrders.length

  return (
    <div className="mt-2 flex flex-wrap content-start items-start">
      {!order?
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); newOrder(); }}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new order</button>
      : [
        deleteOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); deleteOrder() }}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
        ,
        abandonOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); abandonOrder() }}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Abandon order</button>
        ,
        placeOrderPossible &&
          <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={!placeOrderAllowed} onClick={e => {e.preventDefault(); e.stopPropagation(); placeOrder()}}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
      ]}
  </div>
  )
}