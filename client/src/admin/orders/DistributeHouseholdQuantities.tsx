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
                                                      index: number
                                                      updateQuantity: (item: ReconcilingOrderItem, householdId: number, quantity: number) => void
                                                      saveItem: (item: ReconcilingOrderItem) => void
                                                    }
export const DistributeHouseholdQuantities = ({item, index, updateQuantity, saveItem}: DistributeHouseholdQuantitiesProps) => {
  const save = () => saveItem(item)

  return <React.Fragment>
    <tr>
      <td rowSpan={item.householdQuantities.length + 1} className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})}>
      </td>
      <td colSpan={3} className={classNames('pb-4 pl-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        How do you want to share out the new number of items?
      </td>
    </tr>
    {item.householdQuantities.map((h, ix) => {
      const quantities = []

      for(let i = h.minQuantity; i <= h.maxQuantity; i++) {
        quantities.push(i)
      }

      return <tr>
        <td colSpan={2} className={classNames('pb-2 pl-2 align-baseline')}>
          <span className="flex justify-between">
            <span>{h.householdName}</span>
            <span>
              <span className="line-through text-grey-darker mr-2">x {h.oldQuantity}</span>
              <select className="border" value={h.quantity} onChange={e=> updateQuantity(item, h.householdId, parseInt(e.target.value))}>
                {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
            </span>
          </span>
        </td>
        <td className={classNames('pl-2 pr-2 align-bottom text-right')}>
          {ix === item.householdQuantities.length - 1 &&
            <button className="ml-4 whitespace-no-wrap" onClick={_ => save()}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
          }
        </td>
      </tr>
    })}
  </React.Fragment>
}