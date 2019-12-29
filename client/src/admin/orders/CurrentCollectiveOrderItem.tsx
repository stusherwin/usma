import * as React from 'react';
import * as classNames from 'classnames'

import { OrderItem } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Money } from '../../util/Money'

import { ProductFlags } from '../../product/ProductFlags'

export interface CurrentCollectiveOrderItemProps { item: OrderItem
                                                 , index: number 
                                                 }

export const CurrentCollectiveOrderItem = ({item, index}: CurrentCollectiveOrderItemProps) => 
  <React.Fragment>
    <tr key={item.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': index == 0, 'pt-8': index > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${item.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>{item.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': index == 0, 'pt-8': index > 0})}>
        <span>x {item.itemQuantity}</span>
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': index == 0, 'pt-8': index > 0})} colSpan={2}>
        <span><Money amount={item.itemTotalExcVat} /></span>
      </td>
    </tr>
    <tr key={item.productId + '-2'}>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={3}>
        {item.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
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