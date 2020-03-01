import * as React from 'react';

import { HouseholdOrder, ProductCatalogueEntry } from 'util/Types'
import { Icon } from 'util/Icon'

export interface HouseholdOrderButtonsProps { unusedProducts: ProductCatalogueEntry[]
                                              currentHouseholdOrder: HouseholdOrder | undefined
                                              className: string
                                              leaveOrder: () => void
                                              reopenOrder: () => void
                                              abandonOrder: () => void
                                              completeOrder: () => void
                                              startAdd: () => void
                                            }

export const HouseholdOrderButtons = (props: HouseholdOrderButtonsProps) => {
  if(!props.currentHouseholdOrder)
    return null

  if(props.currentHouseholdOrder.isPlaced) 
    return null

  const canAddItem = props.currentHouseholdOrder.isOpen && !!props.unusedProducts.length
  const canLeaveOrder = !props.currentHouseholdOrder.items.length
  const canReopenOrder = !!props.currentHouseholdOrder.items.length && !props.currentHouseholdOrder.isOpen
  const canAbandonOrder = !!props.currentHouseholdOrder.items.length && props.currentHouseholdOrder.isOpen
  const canCompleteOrder = !!props.currentHouseholdOrder.items.length && props.currentHouseholdOrder.isOpen
  const orderButtons = [canLeaveOrder, canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

  if(!orderButtons.some(b => b))
    return null

  return (
    <div className={`${props.className} flex flex-wrap justify-end content-start items-start`}>
      {canLeaveOrder && 
        <button className="flex-no-grow flex-no-shrink ml-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); props.leaveOrder()}}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</button>
      }
      {canReopenOrder &&
        <button className="flex-no-grow flex-no-shrink ml-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); props.reopenOrder()}}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</button>
      }
      {canAbandonOrder &&
        <button className="flex-no-grow flex-no-shrink ml-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); props.abandonOrder()}}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon</button>
      }
      {canCompleteOrder && 
        <button className="flex-no-grow flex-no-shrink ml-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); props.completeOrder()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete</button>
      }
      {canAddItem &&
        <button className="flex-no-grow flex-no-shrink ml-2 mt-2" onClick={e => { e.preventDefault(); e.stopPropagation(); props.startAdd()}}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add items</button>
      }
    </div>
  )
}