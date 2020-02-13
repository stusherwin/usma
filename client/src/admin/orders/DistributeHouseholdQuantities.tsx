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
      <td rowSpan={item.householdQuantities.length + 1} className="pt-4">
      </td>
      <td colSpan={3} className="pb-4 pt-4 pl-2 align-baseline">
        <div className="font-bold">Item quantity changed</div>
        <div className="text-grey-dark mt-1">How do you want to share out {item.itemQuantity == 1? 'this item' : `these ${item.itemQuantity} items`}?</div>
      </td>
    </tr>
    {item.householdQuantities.map((h, ix) => {
      const quantities = []

      for(let i = h.minQuantity; i <= h.maxQuantity; i++) {
        quantities.push(i)
      }

      return <tr>
        <td colSpan={2} className={classNames('pl-2 align-baseline', {'pb-2': ix < item.householdQuantities.length - 1, 'pb-4': ix === item.householdQuantities.length - 1 })}>
          <span className="flex justify-between">
            <span>{h.householdName}</span>
            <span>
              <span className="line-through text-grey-dark mr-2">x {h.oldQuantity}</span>
              <select className="border" value={h.quantity} onChange={e=> updateQuantity(item, h.householdId, parseInt(e.target.value))}>
                {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
            </span>
          </span>
        </td>
        <td className={classNames('pl-2 pr-2 align-bottom text-right', {'pb-2': ix < item.householdQuantities.length - 1, 'pb-4': ix === item.householdQuantities.length - 1 })}>
          {ix === item.householdQuantities.length - 1 &&
            <button className="ml-4 -mt-1 whitespace-no-wrap" onClick={_ => save()}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
          }
        </td>
      </tr>
    })}
  </React.Fragment>
}