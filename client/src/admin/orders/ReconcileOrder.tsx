import * as React from 'react'
import { useState, useEffect } from 'react'
import * as classNames from 'classnames'

import { CollectiveOrder, OrderItem as Item, OrderAdjustment, OrderItemAdjustment, VatRate, HouseholdOrder } from 'util/Types'
import { Icon } from 'util/Icon'

import { OrderItem } from 'order/OrderItem'
import { OrderFooter } from 'order/OrderFooter'

import { DistributeHouseholdQuantities, HouseholdQuantity } from './DistributeHouseholdQuantities'

interface ReconcilingOrderItem {
  productId: number
  productCode: string
  productName: string
  productVatRate: VatRate
  itemQuantity: number
  productPriceExcVat: number
  productPriceIncVat: number
  itemTotalExcVat: number
  itemTotalIncVat: number
  biodynamic: boolean
  fairTrade: boolean
  glutenFree: boolean
  organic: boolean
  addedSugar: boolean
  vegan: boolean
  adjustment: OrderItemAdjustment
  reconciled: boolean
  householdQuantities: HouseholdQuantity[]
  initialItemQuantity: number
}

interface ReconcilingOrder { 
  items: ReconcilingOrderItem[] 
  totalExcVat: number
  totalIncVat: number
  isAbandoned: boolean
  adjustment: OrderAdjustment
}

export interface ReconcileOrderProps { order: CollectiveOrder
                                       past?: boolean
                                       endReconcilingItem: (productId: number, productPriceExcVat: number, households: {householdId: number, itemQuantity: number}[]) => void
                                       endReconcilingOrder: () => void 
                                     }

export const ReconcileOrder = ({order, past, endReconcilingItem, endReconcilingOrder}: ReconcileOrderProps) => {
  const buildHouseholdQuantities = (item: ReconcilingOrderItem, householdOrders: HouseholdOrder[]) => {
    const householdQuantities: HouseholdQuantity[] = []
    let remainingQuantity = item.itemQuantity
    let lastUpdated = 0;

    for(let ho of householdOrders) {
      let found = ho.items.find(i => i.productId === item.productId)
      if(!found) continue

      const oldQuantity = found.adjustment? found.adjustment.oldItemQuantity : found.itemQuantity
      const quantity = item.itemQuantity == item.initialItemQuantity? 
        found.itemQuantity
        : Math.min(oldQuantity, remainingQuantity)

      remainingQuantity -= quantity

      householdQuantities.push({ householdId: ho.householdId
                               , householdName: ho.householdName
                               , quantity
                               , minQuantity: 0
                               , maxQuantity: Math.min(oldQuantity, item.itemQuantity)
                               , oldQuantity: oldQuantity
                               , lastUpdated
                               })
      lastUpdated++
    }

    for(let h of householdQuantities) {
      let otherTotal = householdQuantities.filter(h2 => h2.householdId != h.householdId).reduce((t, h2) => t + h2.maxQuantity, 0)
      h.minQuantity = Math.max(0, item.itemQuantity - otherTotal)
    }

    return householdQuantities
  }

  const newReconcilingOrder = (order: CollectiveOrder) => ({
    items: order.items.map(i => {
      const item: ReconcilingOrderItem = {
        productId: i.productId,
        productCode: i.productCode,
        productName: i.productName,
        productVatRate: i.productVatRate,
        itemQuantity: i.itemQuantity,
        productPriceExcVat: i.productPriceExcVat,
        productPriceIncVat: i.productPriceIncVat,
        itemTotalExcVat: i.itemTotalExcVat,
        itemTotalIncVat: i.itemTotalIncVat,
        biodynamic: i.biodynamic,
        fairTrade: i.fairTrade,
        glutenFree: i.glutenFree,
        organic: i.organic,
        addedSugar: i.addedSugar,
        vegan: i.vegan,
        reconciled: !!i.adjustment,
        adjustment: {
          oldProductPriceExcVat: i.adjustment? i.adjustment.oldProductPriceExcVat : i.productPriceExcVat,
          oldProductPriceIncVat: i.adjustment? i.adjustment.oldProductPriceIncVat : i.productPriceIncVat,
          oldItemQuantity: i.adjustment? i.adjustment.oldItemQuantity : i.itemQuantity,
          oldItemTotalExcVat: i.adjustment? i.adjustment.oldItemTotalExcVat : i.itemTotalExcVat,
          oldItemTotalIncVat: i.adjustment? i.adjustment.oldItemTotalIncVat : i.itemTotalIncVat,
          productDiscontinued: false
        },
        initialItemQuantity: i.itemQuantity,
        householdQuantities: []
      }

      item.householdQuantities = buildHouseholdQuantities(item, order.householdOrders)
     
      return item
    }),
    isAbandoned: order.isAbandoned,
    totalExcVat: order.totalExcVat,
    totalIncVat: order.totalIncVat,
    adjustment: { 
      oldTotalExcVat: order.adjustment? order.adjustment.oldTotalExcVat : order.totalExcVat,
      oldTotalIncVat: order.adjustment? order.adjustment.oldTotalIncVat : order.totalIncVat
    }
  })

  const [reconcilingOrder, setReconcilingOrder] = useState<ReconcilingOrder>(() => newReconcilingOrder(order))

  useEffect(() => setReconcilingOrder(newReconcilingOrder(order)), [order])

  const calculateTotals = (item: ReconcilingOrderItem) => {
    item.itemTotalExcVat = item.productPriceExcVat * item.itemQuantity
    item.itemTotalIncVat = item.productPriceIncVat * item.itemQuantity
    reconcilingOrder.totalExcVat = reconcilingOrder.items.reduce((t, i) => t + i.itemTotalExcVat, 0)
    reconcilingOrder.totalIncVat = reconcilingOrder.items.reduce((t, i) => t + i.itemTotalIncVat, 0)    
  }

  const editItemQuantity = (item: ReconcilingOrderItem, quantity: number) => {
    item.itemQuantity = quantity
    
    item.householdQuantities = buildHouseholdQuantities(item, order.householdOrders)
    calculateTotals(item)

    setReconcilingOrder({...reconcilingOrder})
  }

  const editProductPrice = (item: ReconcilingOrderItem, price: number) => {
    const diff = price - item.productPriceExcVat
    
    item.productPriceExcVat = price
    item.productPriceIncVat += diff
    
    calculateTotals(item)
    
    setReconcilingOrder({...reconcilingOrder})
  }

  const editItem = (item: ReconcilingOrderItem) => {
    item.reconciled = false
    setReconcilingOrder({...reconcilingOrder})
  }

  const updateHouseholdQuantity = (item: ReconcilingOrderItem, householdId: number, quantity: number) => {
    const household = item.householdQuantities.find(h => h.householdId == householdId)
    if(!household) return

    const oldQuantity = household.quantity
    if(quantity == oldQuantity) return

    household.quantity = quantity
    const oldLastUpdated = household.lastUpdated
    household.lastUpdated = 0

    let lastUpdatedIndexes: number[] = []
    for(let i = 0; i < item.householdQuantities.length; i++) {
      let h = item.householdQuantities[i]
      if(h != household && h.lastUpdated < oldLastUpdated) {
        h.lastUpdated++
      }

      lastUpdatedIndexes[h.lastUpdated] = i
    }

    let remainingQuantity = quantity - oldQuantity
    for(let i = lastUpdatedIndexes.length - 1; i >= 0 && Math.abs(remainingQuantity) > 0; i--) {
      let h = item.householdQuantities[lastUpdatedIndexes[i]]

      let oldQuantity = h.quantity
      h.quantity = Math.max(h.minQuantity, Math.min(h.maxQuantity, h.quantity - remainingQuantity))
      remainingQuantity += (h.quantity - oldQuantity)
    }

    setReconcilingOrder({...reconcilingOrder})
  }

  const saveItem = (item: ReconcilingOrderItem) => {
    endReconcilingItem(item.productId, item.productPriceExcVat, item.householdQuantities.map(h => ({ householdId: h.householdId, itemQuantity: h.quantity })))
  }

  const showHouseholdQuantities = (item: ReconcilingOrderItem) => item.itemQuantity != item.adjustment.oldItemQuantity && item.itemQuantity > 0 && item.householdQuantities.length > 1

  return (
    <div className="">
      <div className={classNames("text-white h-24 p-2 pt-4 relative shadow-inner-top", {'bg-list': !past, 'bg-list-sepia': past})}>
        <div className={classNames("bg-img-list bg-no-repeat w-16 h-16 absolute", {'sepia': past})}></div>
        <h2 className="leading-none ml-20">Reconcile order</h2>
        <div className="ml-20 mt-3">
          <button onClick={endReconcilingOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Done</button>
        </div>
      </div>
      <div className={classNames("shadow-inner-top border-t", {'bg-white': !past, 'bg-white-sepia': past})}>
        <table className="border-collapse w-full">
          <tbody>
            {reconcilingOrder.items.map(item => {
              return <React.Fragment>
                <OrderItem key={item.productId}
                           item={item} 
                           past={past}
                           minQuantity={0}
                           maxQuantity={item.adjustment.oldItemQuantity}
                           checkedOff={item.reconciled}
                           editItemQuantity={!item.reconciled && editItemQuantity || undefined}
                           editProductPrice={!item.reconciled && editProductPrice || undefined}
                           saveItem={!item.reconciled && !showHouseholdQuantities(item) && saveItem || undefined}
                           editItem={item.reconciled && editItem || undefined} />
                {!item.reconciled && showHouseholdQuantities(item) &&
                  <DistributeHouseholdQuantities item={item}
                                                 updateQuantity={updateHouseholdQuantity}
                                                 saveItem={saveItem} />
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