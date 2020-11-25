import * as React from 'react';
import { useState } from 'react'
import * as classNames from 'classnames'

import { OrderItem as Item } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Image } from 'util/Image'

import { ProductFlags } from 'product/ProductFlags'

export interface OrderItemProps {
  item: Item
  orderAbandoned?: boolean
  minQuantity?: number
  maxQuantity?: number
  checkedOff?: boolean
  past?: boolean
  packing?: boolean
  editItemQuantity?: (item: Item, quantity: number) => void
  editProductPrice?: (item: Item, price: number) => void
  removeItem?: (item: Item) => void
  addToCurrentOrder?: (item: Item) => void
  saveItem?: (item: Item) => void
  editItem?: (item: Item) => void
  toggleItemPacked?: (item: Item) => void
  showProductImage: (productCode: string) => void
}

export const OrderItem = ({
  item,
  orderAbandoned,
  minQuantity,
  maxQuantity,
  checkedOff,
  past,
  packing,
  editItemQuantity,
  editProductPrice,
  removeItem,
  addToCurrentOrder,
  saveItem,
  editItem,
  toggleItemPacked,
  showProductImage
}: OrderItemProps) => {
  if (minQuantity === undefined) minQuantity = 1
  if (maxQuantity === undefined) maxQuantity = 10

  let quantities = []
  for (let i = minQuantity; i <= maxQuantity; i++) {
    quantities.push(i)
  }

  const [priceStringValue, setPriceStringValue] = useState((item.productPriceExcVat / 100.0).toFixed(2))
  const [priceValid, setPriceValid] = useState(item.productPriceExcVat > 0)
  const parsePrice = (stringValue: string) => Math.floor((parseFloat(stringValue) || 0) * 100)
  const updatePrice = (stringValue: string) => {
    setPriceStringValue(stringValue);
    const price = parsePrice(stringValue)
    const valid = price > 0 && price <= 99900
    setPriceValid(valid)

    if (editProductPrice) {
      if (valid) {
        editProductPrice(item, price)
      } else if (item.adjustment) {
        editProductPrice(item, item.adjustment.oldProductPriceExcVat)
      }
    }
  }

  const bgClass = {
    'bg-grey-light': checkedOff || item.packed,
    'cursor-pointer': packing
  }

  const checkmarkClass = {
    'text-red': checkedOff,
    'text-black': item.packed
  }

  return <React.Fragment>
    <tr onClick={_ => packing && toggleItemPacked && toggleItemPacked(item)}>
      <td className={classNames('w-20 h-20 align-top pl-2 pt-4', bgClass)} rowSpan={editProductPrice ? 4 : 3}>
        <a href="#" onClick={e => { e.preventDefault(); e.stopPropagation(); showProductImage(item.productCode); }}>
          <Image className="w-20 h-20 -ml-1" src={ServerApi.url.productImage(item.productCode)} />
        </a>
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline whitespace-no-wrap pt-4', bgClass)}>
        {(checkedOff || item.packed) && <span className={classNames(checkmarkClass)}>{'\u2713 '} </span>}{item.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline whitespace-no-wrap pt-4', bgClass)}>
        {!!editItemQuantity && !editProductPrice
          ? <select className="border" value={item.itemQuantity} onChange={e => editItemQuantity(item, parseInt(e.target.value))}>
            {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
          </select>
          : <span>
            {!!item.adjustment && item.adjustment.oldItemQuantity != item.itemQuantity ?
              <span className="inline-flex flex-col">
                <span className="line-through text-black">x {item.adjustment.oldItemQuantity}</span>
                <span className="text-red font-bold">x {item.itemQuantity}</span>
              </span>
              : <span>x {item.itemQuantity}</span>
            }
          </span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap pt-4', bgClass)}>
        <div>
          {!!item.adjustment && item.adjustment.oldItemTotalExcVat != item.itemTotalExcVat ?
            <span className="inline-flex flex-col">
              <Money className="line-through text-black" amount={item.adjustment.oldItemTotalExcVat} />
              {!item.adjustment.productDiscontinued &&
                <Money className="text-red font-bold" amount={item.itemTotalExcVat} />
              }
            </span>
            : <Money className={classNames({ "line-through text-black": orderAbandoned })} amount={item.itemTotalExcVat} />
          }
        </div>
      </td>
    </tr>
    {!!editProductPrice &&
      <tr onClick={_ => packing && toggleItemPacked && toggleItemPacked(item)}>
        <td className={classNames("pb-2 pl-2 align-baseline text-right", bgClass)}>
          <div className="flex justify-between items-baseline content-start">
            {editItemQuantity
              ? <select className="border" value={item.itemQuantity} onChange={e => editItemQuantity(item, parseInt(e.target.value))}>
                {quantities.map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
              : <span></span>
            }
            <span className="text-right -mr-1 whitespace-no-wrap">@ &pound;</span>
          </div>
        </td>
        <td colSpan={2} className={classNames("pb-2 pl-2 align-baseline", bgClass)}>
          <input type="text" className={classNames("w-20 mr-2", { 'border': priceValid, 'border-2 border-red': !priceValid })} value={priceStringValue} onChange={e => updatePrice(e.target.value)} />
        </td>
      </tr>
    }
    <tr onClick={_ => packing && toggleItemPacked && toggleItemPacked(item)}>
      <td className={classNames('pb-2 pl-2 align-top', bgClass)} colSpan={removeItem || addToCurrentOrder || saveItem || editItem ? 2 : 3}>
        {item.productName}
      </td>
      {(removeItem || addToCurrentOrder || saveItem || editItem) &&
        <td className={classNames('pl-2 pr-2 align-top text-right', bgClass)}>
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
      }
    </tr>
    <tr onClick={_ => packing && toggleItemPacked && toggleItemPacked(item)}>
      <td className={classNames('`pl-1 pr-1 pb-2`', bgClass)} colSpan={editProductPrice ? 2 : 3}>
        <div className="flex flex-wrap justify-start">
          <span className="pl-1 pr-1 pb-2 whitespace-no-wrap">
            <ProductFlags p={item} />
          </span>
          <span className="pl-1 pr-1 pb-2 text-grey-dark whitespace-no-wrap">VAT: {item.productVatRate} rate</span>
        </div>
      </td>
    </tr>
  </React.Fragment>
}