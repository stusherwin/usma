import * as React from 'react';
import * as classNames from 'classnames'

import { PastOrderItem, PastCollectiveOrder } from '../../util/Types'
import { Money } from '../../util/Money'
import { ServerApi } from '../../util/ServerApi'

import { ProductFlags } from '../../product/ProductFlags'

export interface PastHouseholdOrderItemProps { item: PastOrderItem
                                             , index: number 
                                             , pastOrder: PastCollectiveOrder
                                             }

export const PastHouseholdOrderItem = ({item, index, pastOrder}: PastHouseholdOrderItemProps) => 
  <React.Fragment>
    <tr key={item.productId + '-1'}>
      <td className={classNames('pl-2 w-20 h-20 align-top', {'pt-4': index == 0, 'pt-8': index > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pl-2 pb-2 font-bold align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        {item.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        x {item.itemQuantity}
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': index == 0, 'pt-8': index > 0}, {'line-through text-grey-dark': pastOrder.isAbandoned})} colSpan={2}>
        <Money amount={item.itemTotalExcVat} />
      </td>
    </tr>
    <tr key={item.productId + '-2'}>
      <td className={classNames('pl-2 pb-2 pr-2 align-top')} colSpan={3}>{item.productName}</td>
    </tr>
    <tr key={item.productId + '-3'}>
      <td className={classNames('pl-2 pr-2')} colSpan={3}>
        <span className="pr-2">
          <ProductFlags p={item} />
        </span>
        <span className="text-grey whitespace-no-wrap">VAT: {item.productVatRate} rate</span>
      </td>
    </tr>
  </React.Fragment>
