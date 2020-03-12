import * as React from 'react'
import * as classNames from 'classnames'

import { OrderItem as Item } from 'util/Types'
import { Icon } from 'util/Icon'

interface ReconcilingOrderItem extends Item {
  householdQuantities: HouseholdQuantity[]
}

export interface HouseholdQuantity { householdId: number
                                     householdName: string
                                     quantity: number
                                     minQuantity: number
                                     maxQuantity: number
                                     oldQuantity: number
                                     lastUpdated: number
                                   }

export interface DistributeHouseholdQuantitiesProps { item: ReconcilingOrderItem
                                                      updateQuantity: (item: ReconcilingOrderItem, householdId: number, quantity: number) => void
                                                      saveItem: (item: ReconcilingOrderItem) => void
                                                    }
export const DistributeHouseholdQuantities = ({item, updateQuantity, saveItem}: DistributeHouseholdQuantitiesProps) => {
  const save = () => saveItem(item)

  return <React.Fragment>
    <tr>
      <td colSpan={4} className="pb-4 pt-4 pl-2 align-baseline bg-red-lighter">
        <div className="font-bold">Item quantity changed</div>
        <div className="mt-1">How do you want to share out {item.itemQuantity == 1? 'this item' : `these ${item.itemQuantity} items`}?</div>
      </td>
    </tr>
    {item.householdQuantities.map((h, ix) => {
      const quantities = []

      for(let i = h.minQuantity; i <= h.maxQuantity; i++) {
        quantities.push(i)
      }

      return <tr>
        <td colSpan={2} className={classNames("pl-2 align-baseline bg-red-lighter", {"pb-2": ix < item.householdQuantities.length - 1, "pb-4": ix == item.householdQuantities.length - 1})}>
          <span className="flex justify-between items-baseline">
            <span>{h.householdName}</span>
          </span>
        </td>
        <td colSpan={2} className={classNames("pl-2 pr-2 pb-2 align-baseline bg-red-lighter", {"pb-2": ix < item.householdQuantities.length - 1, "pb-4": ix == item.householdQuantities.length - 1})}>
          <div className="flex justify-end items-baseline">
            <span className="w-10 line-through whitespace-no-wrap">x {h.oldQuantity}</span>
            <select className="ml-1 border" value={h.quantity} onChange={e=> updateQuantity(item, h.householdId, parseInt(e.target.value))}>
              {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          </div>
        </td>
      </tr>
    })}
    <tr>
      <td className="pt-2 pl-2 pb-4" colSpan={3}>
      </td>
      <td className="pt-2 pl-2 pr-2 pb-4 text-right">
        <button className="ml-4 whitespace-no-wrap" onClick={_ => save()}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
      </td>
    </tr>
  </React.Fragment>
}