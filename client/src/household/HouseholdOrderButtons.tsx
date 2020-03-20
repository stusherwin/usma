import * as React from 'react';

import { HouseholdOrder, CollectiveOrder, ProductCatalogueEntry } from 'util/Types'
import { Icon } from 'util/Icon'

export interface HouseholdOrderButtonsProps { unusedProducts: ProductCatalogueEntry[]
                                              currentHouseholdOrder: HouseholdOrder | undefined
                                              collectiveOrder: CollectiveOrder | undefined
                                              className: string
                                              newOrder?: () => void
                                              reopenOrder: () => void
                                              abandonOrder: () => void
                                              completeOrder: () => void
                                              startAdd: () => void
                                            }

export const HouseholdOrderButtons = ({unusedProducts, currentHouseholdOrder, collectiveOrder, className, newOrder, reopenOrder, abandonOrder, completeOrder, startAdd}: HouseholdOrderButtonsProps) => {
  if(!collectiveOrder || collectiveOrder.orderIsPlaced || collectiveOrder.orderIsAbandoned)
    return newOrder &&
      <div className={`${className} flex flex-wrap justify-start content-start items-start`}>
        <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); newOrder(); }}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new order</button>
      </div>
    || <span>Stu</span>

console.log('currentHouseholdOrder:')
    console.log(currentHouseholdOrder)
    console.log(unusedProducts.length)

  const canAddItem = !!unusedProducts.length && (!currentHouseholdOrder || currentHouseholdOrder.isOpen)
  const canReopenOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && !currentHouseholdOrder.isOpen
  const canAbandonOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && currentHouseholdOrder.isOpen
  const canCompleteOrder = !!currentHouseholdOrder && !!currentHouseholdOrder.items.length && currentHouseholdOrder.isOpen
  
  const orderButtons = [canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

  return orderButtons.some(b => b) &&
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
  || <span>Stu2</span>
}