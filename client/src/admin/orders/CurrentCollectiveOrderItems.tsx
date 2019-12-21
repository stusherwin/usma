import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, OrderItem } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Money } from '../../util/Money'
import { Icon } from '../../util/Icon'
import { ProductFlags } from '../../product/ProductList'

export interface CurrentCollectiveOrderItemsProps { currentOrder: CollectiveOrder
                                        }

export interface CurrentCollectiveOrderItemsState {
                                        }
                                     
export class CurrentCollectiveOrderItems extends React.Component<CurrentCollectiveOrderItemsProps, CurrentCollectiveOrderItemsState> {
  render() {
    const order = this.props.currentOrder
  
    return !order.items.length?
      <div className="px-2 py-4 text-grey-darker">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet
      </div>
    : <div>
        <div className="flex justify-end mr-2 mt-4 mb-2">
          <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
        </div>
        <table className="border-collapse w-full">
          {this.props.currentOrder.items.map(this.renderItem)}
          <tr>
            <td></td>
            <td className={classNames('pt-4 align-baseline px-2')} colSpan={4}>
              <div className="flex justify-between">
                <span>VAT:</span>
                <span className={classNames('text-right')}>
                  <span><Money amount={order.totalIncVat - order.totalExcVat} /></span>
                </span>
              </div>
            </td>
          </tr>
          <tr>
            <td></td>
            <td className={classNames('pt-4 align-baseline px-2 pb-4')} colSpan={4}>
              <div className="flex justify-between font-bold">
                <span>Total:</span>
                <span className="text-right">
                  <Money className="flex-no-shrink flex-no-grow text-right" amount={order.totalIncVat} />
                </span>
              </div>
            </td>
          </tr>
        </table>
      </div>
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
      <td className={classNames('pl-2')} colSpan={4}>
        <span className="pr-2">
          <ProductFlags p={i} />
        </span>
        <span className="text-grey whitespace-no-wrap">VAT: {i.productVatRate} rate</span>
      </td>   
      {/* <td className={classNames('pl-2 pr-2')}>&nbsp;</td> */}
    </tr>
    ]
}