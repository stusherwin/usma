import * as React from 'react';
import { useState } from 'react'
import * as classNames from 'classnames'

import { OrderItem as Item } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'

import { ProductFlags } from 'product/ProductFlags'

export interface OrderItemProps { item: Item
                                  orderAbandoned?: boolean
                                  minQuantity?: number
                                  maxQuantity?: number
                                  checkedOff?: boolean
                                  past?: boolean
                                  editItemQuantity?: (item: Item, quantity: number) => void
                                  editProductPrice?: (item: Item, price: number) => void
                                  removeItem?: (item: Item) => void
                                  addToCurrentOrder?: (item: Item) => void
                                  saveItem?: (item: Item) => void
                                  editItem?: (item: Item) => void
                                }

export const OrderItem = ({ item
                          , orderAbandoned
                          , minQuantity
                          , maxQuantity
                          , checkedOff
                          , past
                          , editItemQuantity
                          , editProductPrice
                          , removeItem
                          , addToCurrentOrder
                          , saveItem
                          , editItem
                          }: OrderItemProps) => {
  if(minQuantity === undefined) minQuantity = 1
  if(maxQuantity === undefined) maxQuantity = 10

  let quantities = []
  for(let i = minQuantity; i <= maxQuantity; i++) {
    quantities.push(i)
  }

  const [priceStringValue, setPriceStringValue] = useState((item.productPriceExcVat / 100.0).toFixed(2))
  const [priceValid, setPriceValid] = useState(item.productPriceExcVat > 0)
  const parsePrice = (stringValue: string) => Math.floor((parseFloat(stringValue) || 0) * 100)
  const updatePrice = (stringValue: string) => {
    setPriceStringValue(stringValue);
    const price =  parsePrice(stringValue)
    const valid = price > 0 && price <= 99900
    setPriceValid(valid)

    if(editProductPrice) {
      if(valid) {
        editProductPrice(item, price)
      } else if(item.adjustment) {
        editProductPrice(item, item.adjustment.oldProductPriceExcVat)
      }
    }
  }

  return <React.Fragment>
    <tr>
      <td className={classNames('w-20 h-20 align-top pl-2 pt-4', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})} rowSpan={editProductPrice ? 4 : 3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline whitespace-no-wrap pt-4', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})}>
        {checkedOff && <span className="text-red">{'\u2713 '} </span>}{item.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline whitespace-no-wrap pt-4', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})}>
        {!!editItemQuantity
          ? <select className="border" value={item.itemQuantity} onChange={e => editItemQuantity(item, parseInt(e.target.value))}>
              {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span>
              {!!item.adjustment && item.adjustment.oldItemQuantity != item.itemQuantity?
                <span>
                  <span className="line-through text-black mr-2">x {item.adjustment.oldItemQuantity}</span>
                  <span className="text-red font-bold">x {item.itemQuantity}</span>
                </span>
              : <span>x {item.itemQuantity}</span>
              }
            </span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap pt-4', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})}>
        {editProductPrice &&
          <div>
            @ &pound;<input type="text" className={classNames("w-20", {'border': priceValid, 'border-2 border-red': !priceValid})} value={priceStringValue} onChange={e => updatePrice(e.target.value)} />
          </div>
        }
        <div>
          {!!item.adjustment && item.adjustment.oldItemTotalExcVat != item.itemTotalExcVat?
            <span className="inline-flex flex-col">
              <Money className="line-through text-black" amount={item.adjustment.oldItemTotalExcVat} />
              {!item.adjustment.productDiscontinued && 
                <Money className="text-red font-bold" amount={item.itemTotalExcVat} />
              }
            </span>
          : <Money className={classNames({"line-through text-black": orderAbandoned})} amount={item.itemTotalExcVat} />
          }
        </div>
      </td>
    </tr>
    <tr>
      <td className={classNames('pb-2 pl-2 align-top', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})} colSpan={2}>
        {item.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})}>
        {!!removeItem &&
          <button className="ml-4" onClick={() => removeItem(item)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
        {!!addToCurrentOrder &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => addToCurrentOrder(item)}><Icon type="add" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Add</button>
        }
        {!!saveItem &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => saveItem(item)}><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2" /></button>
        }
        {!!editItem &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => editItem(item)}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
      </td>
    </tr>
    <tr>
      <td className={classNames('pl-2 pb-4', {'bg-list-lightest': !past && checkedOff, 'bg-list-lightest-sepia': past && checkedOff})} colSpan={3}>
        <span className="pr-2">
          <ProductFlags p={item} />
        </span>
        <span className="text-grey-dark whitespace-no-wrap">VAT: {item.productVatRate} rate</span>
      </td>
    </tr>
  </React.Fragment>
}