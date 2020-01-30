import * as React from 'react'
import { useState } from 'react'

import { CollectiveOrder, OrderItem as Item } from 'util/Types'
import { Icon } from 'util/Icon'

import { OrderItem } from 'order/OrderItem'
import { OrderFooter } from 'order/OrderFooter'

import { DistributeHouseholdQuantities, HouseholdQuantity } from './DistributeHouseholdQuantities'

export type ReconcilingQuantities = {[productId: string]: HouseholdQuantity[]}

export interface ReconcileOrderProps { order: CollectiveOrder
                                       endReconcilingItem: (productId: number, productPriceExcVat: number, households: {householdId: number, itemQuantity: number}[]) => void
                                       endReconcilingOrder: () => void 
                                     }

export const ReconcileOrder = ({order, endReconcilingItem, endReconcilingOrder}: ReconcileOrderProps) => {
  const newReconcilingOrder = () => {
    const reconcilingOrder = {...order}

    for(let item of reconcilingOrder.items) {
      if(!item.adjustment) {
        item.adjustment = {
          oldProductPriceExcVat: item.productPriceExcVat,
          oldProductPriceIncVat: item.productPriceIncVat,
          oldItemQuantity: item.itemQuantity,
          oldItemTotalExcVat: item.itemTotalExcVat,
          oldItemTotalIncVat: item.itemTotalIncVat,
          productDiscontinued: false
        }
      }
    }

    if(!reconcilingOrder.adjustment) {
      reconcilingOrder.adjustment = {
        oldTotalExcVat: reconcilingOrder.totalExcVat,
        oldTotalIncVat: reconcilingOrder.totalIncVat
      }
    }

    return reconcilingOrder
  }

  const [reconcilingOrder, setReconcilingOrder] = useState<CollectiveOrder>(newReconcilingOrder)
  const [reconcilingQuantities, setReconcilingQuantities] = useState<ReconcilingQuantities>({})

  const calculateTotals = (item: Item) => {
    item.itemTotalExcVat = item.productPriceExcVat * item.itemQuantity
    item.itemTotalIncVat = item.productPriceIncVat * item.itemQuantity
    reconcilingOrder.totalExcVat = reconcilingOrder.items.reduce((t, i) => t + i.itemTotalExcVat, 0)
    reconcilingOrder.totalIncVat = reconcilingOrder.items.reduce((t, i) => t + i.itemTotalIncVat, 0)    
  }

  const editItemQuantity = (item: Item, quantity: number) => {
    item.itemQuantity = quantity
    
    calculateTotals(item)

    setReconcilingOrder({...reconcilingOrder})
  }

  const editProductPrice = (item: Item, price: number) => {
    const diff = price - item.productPriceExcVat
    
    item.productPriceExcVat = price
    item.productPriceIncVat += diff
    
    calculateTotals(item)
    
    setReconcilingOrder({...reconcilingOrder})
  }

  const saveItem = (item: Item) => {
    const households: HouseholdQuantity[] = []
    let quantityRemaining = item.itemQuantity
    let lastUpdated = 0;

    for(let ho of reconcilingOrder.householdOrders) {
      let found = ho.items.find(i => i.productId === item.productId)
      if(!found) continue

      const oldQuantity = found.adjustment? found.adjustment.oldItemQuantity : found.itemQuantity
      const quantity = Math.min(oldQuantity, quantityRemaining)
      quantityRemaining -= quantity

      households.push({ householdId: ho.householdId
                      , householdName: ho.householdName
                      , quantity
                      , minQuantity: 0
                      , maxQuantity: Math.min(oldQuantity, item.itemQuantity)
                      , lastUpdated
                      })
      lastUpdated++
    }

    if(item.adjustment && item.itemQuantity != item.adjustment.oldItemQuantity && item.itemQuantity > 0 && households.length > 1) {
      for(let h of households) {
        let otherTotal = households.reduce((t, h2) => h2.householdId == h.householdId? t : t + h2.maxQuantity, 0)
        h.minQuantity = Math.max(0, item.itemQuantity - otherTotal)
      }
      reconcilingQuantities[item.productId] = households
    } else {
      endReconcilingItem(item.productId, item.productPriceExcVat, households.map(h => ({ householdId: h.householdId, itemQuantity: h.quantity })))
    }

    setReconcilingOrder({...reconcilingOrder})
    setReconcilingQuantities({...reconcilingQuantities})
  }

  const editItem = (item: Item) => {
    item.reconciled = false
    delete reconcilingQuantities[item.productId]

    setReconcilingOrder({...reconcilingOrder})
    setReconcilingQuantities({...reconcilingQuantities})
  }

  const updateQuantity = (item: Item, householdId: number, quantity: number) => {
    const itemQuantities = reconcilingQuantities[item.productId]
    const household = itemQuantities.find(h => h.householdId == householdId)
    if(!household) return

    const oldQuantity = household.quantity
    if(quantity == oldQuantity) return

    household.quantity = quantity
    const oldLastUpdated = household.lastUpdated
    household.lastUpdated = 0

    let lastUpdatedIndexes: number[] = []
    for(let i = 0; i < itemQuantities.length; i++) {
      let h = itemQuantities[i]
      if(h != household && h.lastUpdated < oldLastUpdated) {
        h.lastUpdated++
      }

      lastUpdatedIndexes[h.lastUpdated] = i
    }

    let remainingQuantity = quantity - oldQuantity
    for(let i = lastUpdatedIndexes.length - 1; i >= 0 && Math.abs(remainingQuantity) > 0; i--) {
      let h = itemQuantities[lastUpdatedIndexes[i]]

      let oldQuantity = h.quantity
      h.quantity = Math.max(h.minQuantity, Math.min(h.maxQuantity, h.quantity - remainingQuantity))
      remainingQuantity += (h.quantity - oldQuantity)
    }

    setReconcilingQuantities({...reconcilingQuantities})
  }

  return (
    <div className="">
      <div className="bg-product-light text-white p-2 relative shadow-inner-top">
        <div className="bg-img-product bg-no-repeat w-16 h-16 absolute"></div>
        <h2 className="leading-none ml-20">Reconcile order</h2>
        <div className="ml-20 mt-3">
          <button onClick={endReconcilingOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Done</button>
        </div>
      </div>
      <div className="shadow-inner-top border-t bg-white">
        <table className="border-collapse w-full">
          <tbody>
            {reconcilingOrder.items.map((item, index) => {
              let maxQuantity = item.adjustment? item.adjustment.oldItemQuantity : item.itemQuantity
              return <React.Fragment>
                <OrderItem key={item.productId}
                           item={item} 
                           index={index}
                           minQuantity={0}
                           maxQuantity={maxQuantity}
                           checkedOff={item.reconciled}
                           editItemQuantity={!item.reconciled && !reconcilingQuantities[item.productId] && editItemQuantity || undefined}
                           editProductPrice={!item.reconciled && !reconcilingQuantities[item.productId] && editProductPrice || undefined}
                           saveItem={!item.reconciled && !reconcilingQuantities[item.productId] && saveItem || undefined}
                           editItem={(item.reconciled || reconcilingQuantities[item.productId]) && editItem || undefined} />
                {!item.reconciled && reconcilingQuantities[item.productId] &&
                  <DistributeHouseholdQuantities item={item}
                                                 index={index}
                                                 householdQuantities={reconcilingQuantities[item.productId]} 
                                                 updateQuantity={updateQuantity}
                                                 saveHouseholdQuantities={endReconcilingItem} />
                }
              </React.Fragment>
            })}
            <OrderFooter order={reconcilingOrder} />
          </tbody>
        </table>
      </div>
    </div>
  )
}