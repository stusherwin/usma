import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, HouseholdOrderItem } from '../util/Types'
import { ServerApi } from '../util/ServerApi'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'

import { ProductFlags } from '../product/ProductFlags'

export interface CurrentHouseholdOrderItemProps { householdOrder: HouseholdOrder,
                                                  item: HouseholdOrderItem,
                                                  index: number,
                                                  readOnly?: boolean,
                                                  editQuantity: (item: HouseholdOrderItem, quantity: number) => void 
                                                  removeItem: (item: HouseholdOrderItem) => void 
                                                }

export const CurrentHouseholdOrderItem = ({item, householdOrder, index, readOnly, editQuantity, removeItem}: CurrentHouseholdOrderItemProps) => 
  <React.Fragment>
    <tr key={item.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': index == 0, 'pt-8': index > 0, '': item.productDiscontinued})}>{item.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {!readOnly && householdOrder.isOpen && !item.productDiscontinued
          ? <select className="border" value={item.itemQuantity} onChange={e => editQuantity(item, parseInt(e.target.value))}>
              {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span className={classNames({'': item.productDiscontinued})}>x {item.itemQuantity}</span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': index == 0, 'pt-8': index > 0})} colSpan={2}>
        {item.oldItemTotalExcVat !== null && item.oldItemTotalExcVat != item.itemTotalExcVat?
          <span>
            <span className="line-through"><Money amount={item.oldItemTotalExcVat} /></span> 
            {!item.productDiscontinued && 
              <Money className="text-red font-bold" amount={item.itemTotalExcVat} />
            }
          </span>
        : <span className={classNames({"line-through text-grey-dark": householdOrder.isAbandoned})}><Money amount={item.itemTotalExcVat} /></span>
        }
      </td>
    </tr>
    <tr key={item.productId + '-2'}>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={3}>
        {item.productDiscontinued
          ? <span>
              <span className="">{item.productName}</span><br />
            </span>
          : item.productName
        }
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
        {!readOnly && householdOrder.isOpen && !item.productDiscontinued &&
          <button className="ml-4" onClick={() => removeItem(item)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
      </td>
    </tr>
    <tr key={item.productId + '-3'}>
      <td className={classNames('pl-2')} colSpan={4}>
        <span className="pr-2">
        <ProductFlags p={item} />
        </span>
        <span className="text-grey whitespace-no-wrap">VAT: {item.productVatRate} rate</span>
      </td>
      {/* <td className={classNames('pl-2 pr-2')}>&nbsp;</td> */}
    </tr>
  </React.Fragment>