import * as React from 'react';

import { HouseholdOrder, ProductCatalogueEntry } from 'util/Types'
import { Icon } from 'util/Icon'

export interface HouseholdOrderButtonsProps { unusedProducts: ProductCatalogueEntry[]
                                              currentHouseholdOrder: HouseholdOrder | undefined
                                              className: string
                                              newOrder?: () => void
                                              reopenOrder: () => void
                                              abandonOrder: () => void
                                              completeOrder: () => void
                                              startAdd: () => void
                                            }

export const HouseholdOrderButtons = ({unusedProducts, currentHouseholdOrder, className, newOrder, reopenOrder, abandonOrder, completeOrder, startAdd}: HouseholdOrderButtonsProps) => {
  if(currentHouseholdOrder && (currentHouseholdOrder.orderIsPlaced || currentHouseholdOrder.orderIsAbandoned)) 
    return !newOrder?
      null
    : <div className={`${className} flex flex-wrap justify-start content-start items-start`}>
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); newOrder(); }}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new order</button>
      </div>

  const canAddItem = !currentHouseholdOrder || currentHouseholdOrder.isOpen && !!unusedProducts.length
  const canReopenOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && !currentHouseholdOrder.isOpen
  const canAbandonOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && currentHouseholdOrder.isOpen
  const canCompleteOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && currentHouseholdOrder.isOpen
  const orderButtons = [canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

  if(!orderButtons.some(b => b))
    return null

  return (
    <div className={`${className} flex flex-wrap justify-start content-start items-start`}>
      {canReopenOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); reopenOrder()}}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</button>
      }
      {canAbandonOrder &&
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); abandonOrder()}}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon</button>
      }
      {canCompleteOrder && 
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); completeOrder()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete</button>
      }
      {canAddItem &&
        <button className="flex-no-grow flex-no-shrink mt-2 ml-auto" onClick={e => { e.preventDefault(); e.stopPropagation(); startAdd()}}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add items</button>
      }
    </div>
  )
}