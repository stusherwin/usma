import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, OrderItem } from '../Types'
import { ServerApi } from '../ServerApi'
import { Money } from '../common/Money'
import { Icon } from '../common/Icon'
import { ProductFlags } from './ProductList'

export interface CurrentOrderItemsProps { currentOrder: CollectiveOrder
                                        }

export interface CurrentOrderItemsState {
                                        }
                                     
export class CurrentOrderItems extends React.Component<CurrentOrderItemsProps, CurrentOrderItemsState> {
  render() {
    const currentOrder = this.props.currentOrder
  
    return (
      <div>
        <div className="flex justify-end mr-2 mb-2">
          <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
        </div>
        <table className="border-collapse w-full">
          {this.props.currentOrder.items.map(this.renderItem)}
          <tr>
            <td className={classNames('pt-4 align-baseline px-2')} colSpan={5}>
              <div className="flex justify-end">
                <span>VAT:</span>
                <span className={classNames('w-24 text-right')}>
                  <span><Money amount={currentOrder.totalIncVat - currentOrder.totalExcVat} /></span>
                </span>
              </div>
            </td>
          </tr>
          <td className={classNames('pt-4 align-baseline px-2 pb-4')} colSpan={5}>
            <div className="flex justify-end font-bold">
              <span>Total:</span>
              <span className="w-24 text-right">
                <Money className="flex-no-shrink flex-no-grow text-right" amount={currentOrder.totalIncVat} />
              </span>
            </div>
          </td>
        </table>
      </div>
    )
  }

  renderItem = (i: OrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': ix == 0, 'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${i.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>{i.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>
        <span>x {i.itemQuantity}</span>
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': ix == 0, 'pt-8': ix > 0})} colSpan={2}>
        <span><Money amount={i.itemTotalExcVat} /></span>
      </td>
    </tr>
    ,
    <tr key={i.productId + '-2'}>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={3}>
        {i.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
      </td>
    </tr>
    ,
    <tr key={i.productId + '-3'}>
      <td className={classNames('pl-2')} colSpan={3}>
        <ProductFlags p={i} />
        <span className="text-grey pl-4 whitespace-no-wrap">VAT: {i.productVatRate} rate</span>
      </td>   
      <td className={classNames('pl-2 pr-2')}>&nbsp;</td>
    </tr>
    ]
}