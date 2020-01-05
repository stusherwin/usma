import * as React from 'react';
import * as classNames from 'classnames'

import { OrderItem } from '../util/Types'
import { ServerApi } from '../util/ServerApi'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'

import { ProductFlags } from '../product/ProductFlags'

export interface CurrentHouseholdOrderItemProps { item: OrderItem
                                                  index: number
                                                  orderAbandoned?: boolean
                                                  canEditQuantity?: boolean
                                                  editQuantity?: (item: OrderItem, quantity: number) => void
                                                  canRemoveItem?: boolean
                                                  removeItem?: (item: OrderItem) => void
                                                  canAddToCurrentOrder?: boolean
                                                  addToCurrentOrder?: (item: OrderItem) => void
                                                }

export const CurrentHouseholdOrderItem = ({item, index, orderAbandoned, canEditQuantity, editQuantity, canRemoveItem, removeItem, canAddToCurrentOrder, addToCurrentOrder}: CurrentHouseholdOrderItemProps) =>
  <React.Fragment>
    <tr>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {item.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {canEditQuantity
          ? <select className="border" value={item.itemQuantity} onChange={e => !!editQuantity && editQuantity(item, parseInt(e.target.value))}>
              {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span>x {item.itemQuantity}</span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {!(item.oldItemTotalExcVat === null || item.oldItemTotalExcVat === undefined) && item.oldItemTotalExcVat != item.itemTotalExcVat?
          <span>
            <span className="line-through"><Money amount={item.oldItemTotalExcVat} /></span> 
            {!item.productDiscontinued && 
              <Money className="text-red font-bold" amount={item.itemTotalExcVat} />
            }
          </span>
        : <span className={classNames({"line-through text-grey-dark": orderAbandoned})}><Money amount={item.itemTotalExcVat} /></span>
        }
      </td>
    </tr>
    <tr>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={2}>
        {item.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
        {canRemoveItem &&
          <button className="ml-4" onClick={() => !!removeItem && removeItem(item)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
        {canAddToCurrentOrder &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => addToCurrentOrder && addToCurrentOrder(item)}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add</button>
        }
      </td>
    </tr>
    <tr>
      <td className={classNames('pl-2')} colSpan={3}>
        <span className="pr-2">
          <ProductFlags p={item} />
        </span>
        <span className="text-grey whitespace-no-wrap">VAT: {item.productVatRate} rate</span>
      </td>
    </tr>
  </React.Fragment>