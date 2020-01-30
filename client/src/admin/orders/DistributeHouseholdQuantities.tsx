import * as React from 'react'
import * as classNames from 'classnames'

import { OrderItem as Item } from 'util/Types'
import { Icon } from 'util/Icon'

export interface HouseholdQuantity { householdId: number
                                     householdName: string
                                     quantity: number
                                     minQuantity: number
                                     maxQuantity: number
                                     lastUpdated: number
                                   }

export interface DistributeHouseholdQuantitiesProps { item: Item
                                                      index: number
                                                      householdQuantities: HouseholdQuantity[]
                                                      updateQuantity: (item: Item, householdId: number, quantity: number) => void
                                                    }
export const DistributeHouseholdQuantities = ({item, index, householdQuantities, updateQuantity}: DistributeHouseholdQuantitiesProps) => {
  return <React.Fragment>
    <tr>
      <td rowSpan={householdQuantities.length + 1} className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})}>
      </td>
      <td colSpan={3} className={classNames('pb-4 pl-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        How do you want to distribute the items?
      </td>
    </tr>
    {householdQuantities.map((h, ix) => {
      const quantities = []

      for(let i = h.minQuantity; i <= h.maxQuantity; i++) {
        quantities.push(i)
      }

      return <tr>
        <td colSpan={2} className={classNames('pb-2 pl-2 align-baseline')}>
          <span className="flex justify-between">
            <span>{h.householdName}</span>
            <span>
              <span className="line-through text-grey-darker mr-2">x {h.maxQuantity}</span>
              <select className="border" value={h.quantity} onChange={e=> updateQuantity(item, h.householdId, parseInt(e.target.value))}>
                {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
            </span>
          </span>
        </td>
        <td className={classNames('pl-2 pr-2 align-bottom text-right')}>
          {ix === householdQuantities.length - 1 &&
            <button className="ml-4 whitespace-no-wrap" onClick={() => {}}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
          }
        </td>
      </tr>
    })}
  </React.Fragment>
}