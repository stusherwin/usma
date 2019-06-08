import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder, PastCollectiveOrder, PastOrderItem } from '../Types'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible } from '../common/Collapsible'
import { ServerApi } from '../ServerApi'

export interface PastHouseholdOrdersProps { pastOrder: PastCollectiveOrder 
                                          , pastHouseholdOrders: PastHouseholdOrder[]
                                          }

export interface PastHouseholdOrdersState { expanded: PastHouseholdOrder | null }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, PastHouseholdOrdersState> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = { 
      expanded: null, 
    }
  }

  toggle = (toExpand: PastHouseholdOrder) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    return (
      <table className="border-collapse w-full bg-grey-lighter shadow-inner-top">
        <tbody>
          {this.props.pastHouseholdOrders.map((ho, i) => {
            return (
              <tr key={ho.householdId}>
                <td colSpan={2}>
                  <Collapsible className="min-h-24"
                               expanded={this.state.expanded == ho}
                               otherExpanding={!!this.state.expanded && this.state.expanded != ho}
                               toggle={this.toggle(ho)}
                               header={() =>
                                 <div className={classNames('p-2 bg-household-lighter min-h-24', {'shadow-inner-top': i == 0})}>
                                   <div className="bg-no-repeat w-16 h-16 absolute bg-img-household mt-2"></div>
                                   <h3 className="leading-none ml-20 relative flex mt-2">
                                     {ho.householdName}
                                   </h3>
                                   <h4 className="flex justify-end ml-20 mt-4 mb-4">
                                     <span>Total:</span>
                                     <span className={classNames("w-24 font-bold text-right", {'line-through text-grey-darker': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                   </h4>
                                 </div>
                               }>
                    <div className="shadow-inner-top bg-white">
                      {!ho.items.length?
                        <div className="px-2 py-4 text-grey-darker">
                          <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                        </div>
                      : <table className="border-collapse w-full bg-grey-lighter shadow-inner-top">
                          <tbody>
                            {ho.items.map(this.renderItem)}
                            <tr>
                              <td className="pt-2 pr-2 pl-2" colSpan={4}>
                                <div className="flex justify-end">
                                  <span>VAT:</span>
                                  <span className={classNames('w-24 text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat - ho.totalExcVat} /></span>
                                </div>
                              </td>
                            </tr>
                            <tr>
                              <td className="pt-4 pb-4 pl-2 pr-2 font-bold" colSpan={4}>
                                <div className="flex justify-end">
                                  <span>Total:</span>
                                  <span className={classNames('w-24 text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                </div>
                              </td>
                            </tr>
                          </tbody>
                        </table>
                      } 
                    </div>
                  </Collapsible>
                </td>
              </tr>
            )}
          )}
          <tr>
            <td className="pt-4 pb-4 pl-2 pr-2 font-bold" colSpan={2}>
              <div className="flex justify-end">
                <span>Total:</span>
                <span className={classNames('w-24 text-right', {'line-through text-grey-dark': this.props.pastOrder.isAbandoned})}><Money amount={this.props.pastOrder.totalIncVat} /></span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    )
  }  

  renderItem = (i: PastOrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('pl-2 w-20 h-20 align-top', {'pt-4': ix == 0, 'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${i.productCode}`)} />
      </td>
      <td className={classNames('pl-2 pb-2 font-bold align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>
        {i.productCode}
      </td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>
        x {i.itemQuantity}
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': ix == 0, 'pt-8': ix > 0}, {'line-through text-grey-dark': this.props.pastOrder.isAbandoned})} colSpan={2}>
        <Money amount={i.itemTotalExcVat} />
      </td>
    </tr>
    ,
    <tr key={i.productId + '-2'}>
      <td className={classNames('pl-2 pb-2 pr-2 align-top')} colSpan={3}>{i.productName}</td>
    </tr>
    ,
    <tr key={i.productId + '-3'}>
      <td className={classNames('pl-2 pr-2 text-grey')} colSpan={3}>VAT: {i.productVatRate} rate</td>
    </tr>
    ]
}