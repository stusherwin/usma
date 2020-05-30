import * as React from 'react';

import { CollectiveOrder, HouseholdOrder } from 'util/Types'
import { Icon } from 'util/Icon'

interface CollectiveOrderButtonsProps {
  order: CollectiveOrder | undefined
  newOrder?: () => void
  abandonOrder?: () => void
  placeOrder?: () => void
  reconcileOrder?: () => void
  uploadOrderFile?: () => void
}

export const CollectiveOrderButtons = ({order, newOrder, abandonOrder, placeOrder, reconcileOrder, uploadOrderFile}: CollectiveOrderButtonsProps) => {
  const newOrderPossible = !order || order.orderIsPlaced || order.orderIsAbandoned
  const allComplete = !!order && order.isComplete;
  const orderMinimumReached = !!order && order.totalIncVat >= 25000
  const placeOrderPossible = !!order && !order.orderIsPlaced && !order.orderIsAbandoned && !!order.householdOrders.length && allComplete /*&& allPaid*/ && orderMinimumReached

  const abandonOrderPossible = !!order && !order.orderIsPlaced && !order.orderIsAbandoned && !!order.householdOrders.length
  const reconcileOrderPossible = !!order && order.orderIsPlaced
  const uploadOrderFilePossible = !!order && order.orderIsPlaced

  return (
    <div className="flex flex-wrap justify-start content-start items-start">
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
      {uploadOrderFilePossible && uploadOrderFile &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); uploadOrderFile(); }}><Icon type="upload" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Upload order file to reconcile</button>
      }
  </div>
  )
}