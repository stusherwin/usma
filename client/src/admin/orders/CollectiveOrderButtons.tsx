import * as React from 'react';

import { CollectiveOrder, HouseholdOrder } from 'util/Types'
import { Icon } from 'util/Icon'

interface CollectiveOrderButtonsProps {
  order: CollectiveOrder | undefined
  newOrder?: () => void
  deleteOrder?: () => void
  abandonOrder?: () => void
  placeOrder?: () => void
  reconcileOrder?: () => void
}

export const CollectiveOrderButtons = ({order, newOrder, deleteOrder, abandonOrder, placeOrder, reconcileOrder}: CollectiveOrderButtonsProps) => {
  const newOrderPossible = !order || order.isPlaced || order.isAbandoned
  // const deleteOrderPossible = !!order && !order.isPlaced && !order.householdOrders.length
  const allComplete = !!order && order.householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
  const orderMinimumReached = !!order && order.totalIncVat >= 25000
  const placeOrderPossible = !!order && !order.isPlaced && !order.isAbandoned && !!order.householdOrders.length && allComplete /*&& allPaid*/ && orderMinimumReached

  // const placeOrderAllowed = !!order && !order.isPlaced && !order.isAbandoned && allComplete /*&& allPaid*/ && orderMinimumReached

  const abandonOrderPossible = !!order && !order.isPlaced && !order.isAbandoned && !!order.householdOrders.length
  const reconcileOrderPossible = !!order && order.isPlaced

  return (
    <div className="flex flex-wrap justify-start content-start items-start">
      {/* {deleteOrderPossible && deleteOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); deleteOrder() }}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Delete order</button>
      } */}
      {reconcileOrderPossible && reconcileOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); reconcileOrder()}}><Icon type="clipboard-question" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Reconcile order</button>
      }
      {abandonOrderPossible && abandonOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); abandonOrder() }}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Abandon order</button>
      }
      {placeOrderPossible && placeOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); placeOrder()}}><Icon type="cart" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Place order</button>
      }
      {newOrderPossible && newOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); newOrder(); }}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new order</button>
      }
  </div>
  )
}