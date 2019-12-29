import * as React from 'react';
import * as classNames from 'classnames'

import { PastOrderItem } from '../util/Types'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'
import { ServerApi } from '../util/ServerApi'

import { ProductFlags } from '../product/ProductFlags'

export interface PastHouseholdOrderItemProps { item: PastOrderItem
                                             , index: number
                                             , orderAbandoned: boolean
                                             , canAddToCurrentOrder?: boolean
                                             , addToCurrentOrder?: (item: PastOrderItem) => void
                                             }
   
export class PastHouseholdOrderItem extends React.Component<PastHouseholdOrderItemProps, {}> {  
  render() { 
    return <React.Fragment>
      <tr key={this.props.item.productId + '-1'}>
        <td className={classNames('pl-2 w-20 h-20 align-top', {'pt-4': this.props.index == 0, 'pt-8': this.props.index > 0})} rowSpan={3}>
          <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${this.props.item.productCode}`)} />
        </td>
        <td className={classNames('pl-2 pb-2 font-bold align-baseline', {'pt-4': this.props.index == 0, 'pt-8': this.props.index > 0})}>
          {this.props.item.productCode}
        </td>
        <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': this.props.index == 0, 'pt-8': this.props.index > 0})}>
          x {this.props.item.itemQuantity}
        </td>
        <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': this.props.index == 0, 'pt-8': this.props.index > 0}, {'line-through text-grey-dark': this.props.orderAbandoned})} colSpan={2}>
          <Money amount={this.props.item.itemTotalExcVat} />
        </td>
      </tr>
      <tr key={this.props.item.productId + '-2'}>
        <td className={classNames('pl-2 pb-2 align-top')} colSpan={2}>{this.props.item.productName}</td>
        <td className={classNames('pl-2 pr-2 align-top text-right')}>
          {this.props.canAddToCurrentOrder &&
            <button className="ml-4 whitespace-no-wrap" onClick={() => this.props.addToCurrentOrder && this.props.addToCurrentOrder(this.props.item)}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add</button>
          }
        </td>
      </tr>
      <tr key={this.props.item.productId + '-3'}>
        <td className={classNames('pl-2 pr-2')} colSpan={3}>
          <span className="pr-2">
            <ProductFlags p={this.props.item} />
          </span>
          <span className="text-grey whitespace-no-wrap">VAT: {this.props.item.productVatRate} rate</span>
        </td>
      </tr>
    </React.Fragment>
  }
}