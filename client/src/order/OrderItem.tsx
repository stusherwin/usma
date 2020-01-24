import * as React from 'react';
import { useState } from 'react'
import * as classNames from 'classnames'

import { OrderItem as Item } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'

import { ProductFlags } from 'product/ProductFlags'

export interface OrderItemProps { item: Item
                                  index: number
                                  orderAbandoned?: boolean
                                  allowZeroQuantity?: boolean
                                  editItemQuantity?: (item: Item, quantity: number) => void
                                  editProductPrice?: (item: Item, price: number) => void
                                  removeItem?: (item: Item) => void
                                  addToCurrentOrder?: (item: Item) => void
                                }

export const OrderItem = ({ item
                          , index
                          , orderAbandoned
                          , allowZeroQuantity
                          , editItemQuantity
                          , editProductPrice
                          , removeItem
                          , addToCurrentOrder
                          }: OrderItemProps) => {
  let quantities = [1,2,3,4,5,6,7,8,9,10]
  if(allowZeroQuantity) {
    quantities.unshift(0)
  }

  const [priceStringValue, setPriceStringValue] = useState((item.productPriceExcVat / 100.0).toFixed(2))
  const parsePrice = (stringValue: string) => Math.floor((parseFloat(stringValue) || 0) * 100)

  return <React.Fragment>
    <tr>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {item.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {!!editItemQuantity
          ? <select className="border" value={item.itemQuantity} onChange={e => editItemQuantity(item, parseInt(e.target.value))}>
              {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span>x {item.itemQuantity}</span>
        }
        {editProductPrice &&
          <span> @ &pound;<input type="text" className="border w-20" value={priceStringValue} onChange={e => { setPriceStringValue(e.target.value); editProductPrice(item, parsePrice(e.target.value))}} /></span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {item.adjustment != null && item.adjustment.oldItemTotalExcVat != item.itemTotalExcVat?
          <span>
            <Money className="line-through text-grey-darker mr-2" amount={item.adjustment.oldItemTotalExcVat} />
            {!item.adjustment.productDiscontinued && 
              <Money className="text-red font-bold" amount={item.itemTotalExcVat} />
            }
          </span>
        : <Money className={classNames({"line-through text-grey-darker": orderAbandoned})} amount={item.itemTotalExcVat} />
        }
      </td>
    </tr>
    <tr>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={2}>
        {item.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
        {!!removeItem &&
          <button className="ml-4" onClick={() => removeItem(item)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
        {!!addToCurrentOrder &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => addToCurrentOrder(item)}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add</button>
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
}