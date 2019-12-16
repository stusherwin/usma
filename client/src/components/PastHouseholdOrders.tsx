import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder, PastOrderItem, HouseholdOrder } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible, CollapsibleState } from '../common/Collapsible'
import { ServerApi } from '../ServerApi'
import { ProductFlags } from './ProductList'

export interface PastHouseholdOrdersProps { pastHouseholdOrders: PastHouseholdOrder[]
                                          , currentHouseholdOrder: HouseholdOrder | null
                                          , collapsibleKey: string
                                          , collapsibleState: CollapsibleState
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export interface HouseholdPaymentsState { collapsibleState: CollapsibleState
                                        }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, HouseholdPaymentsState> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
    }
  }

  addToCurrentOrder = (item: PastOrderItem) => {
    if(!this.props.currentHouseholdOrder) return;

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, item.productCode, null))
      .then(this.props.reload)
  }

  addAllItemsToCurrentOrder = (ho: PastHouseholdOrder) => {
    if(!this.props.currentHouseholdOrder) return;

    this.props.request(ServerApi.command.ensureAllItemsFromPastHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, ho.orderId))
      .then(this.props.reload)
  }

  render() {
    const pastOrders = this.props.pastHouseholdOrders
    const total = pastOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)

    return (
      <Collapsible className="min-h-20"
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={
                     <div className="p-2 bg-past-order-lighter min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Past orders
                       </h2>
                       <h3 className="flex justify-end ml-20 mt-4">
                         {/* <span>Total:</span> */}
                         <span className="w-24 text-right"><Money amount={total} /></span>
                       </h3>
                     </div>
                   }>
        <div className="shadow-inner-top bg-order-lightest">
          { !pastOrders.length
          ? <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div>
          : <table className="border-collapse w-full">
              <tbody>
                { pastOrders.map((ho, i) => {
                  let status = 
                    <span>
                      { ho.isAbandoned?
                        <span><Icon type="cancel" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Abandoned</span>
                      : <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Complete</span>
                      }
                    </span>

                  return (
                    <tr key={ho.orderId}>
                      <td>
                        <Collapsible className="min-h-20"
                                     collapsibleKey={ho.orderId}
                                     collapsibleState={this.state.collapsibleState}
                                     header={
                                       <div className={classNames('p-2 bg-order-lightest min-h-20', {"shadow-inner-top": i == 0})}>
                                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                                         <h3 className="leading-none ml-20 relative flex">
                                           {Util.formatDate(ho.orderCreatedDate)}
                                         </h3>
                                         <h4 className="flex justify-between ml-20 mt-4 mb-4">
                                           {status}
                                           <span className="flex justify-end">
                                             {/* <span>Total:</span> */}
                                             <span className={classNames("w-24 font-bold text-right", {'line-through text-grey-darker': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                           </span>
                                         </h4>
                                         {this.props.currentHouseholdOrder && this.props.currentHouseholdOrder.isOpen && !!ho.items.length &&
                                           <div className="flex justify-end pt-2">
                                             <button onClick={e => {e.stopPropagation(); e.preventDefault(); this.addAllItemsToCurrentOrder(ho)}}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add all items to current order</button>
                                           </div>
                                         }
                                       </div>
                                     }>
                          <div className="shadow-inner-top bg-grey-lighter">
                            {!ho.items.length?
                              <div className="px-2 py-4 text-grey-darker">
                                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                              </div>
                            : <div>
                                <table className="border-collapse w-full">
                                  <tbody>
                                    {ho.items.map(this.renderItem)}
                                    <tr>
                                      <td></td>
                                      <td className="pt-2 pr-2 pl-2" colSpan={3}>
                                        <div className="flex justify-between">
                                          <span>VAT:</span>
                                          <span className={classNames('text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat - ho.totalExcVat} /></span>
                                        </div>
                                      </td>
                                    </tr>
                                    <tr>
                                      <td></td>
                                      <td className="pt-4 pb-4 pl-2 pr-2 font-bold" colSpan={3}>
                                        <div className="flex justify-between">
                                          <span>Total:</span>
                                          <span className={classNames('text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></span>
                                        </div>
                                      </td>
                                    </tr>
                                  </tbody>
                                </table>
                              </div>
                            }
                          </div>
                        </Collapsible>
                    </td>
                  </tr>
                )}) }
                <tr>
                  <td className="pt-4 pl-20 pr-2 pb-4 font-bold">
                    <div className="pl-2 flex justify-between">
                      <span>Total:</span>
                      <span className="w-24 font-bold text-right"><Money amount={total} /></span>
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>
          }
        </div>
      </Collapsible>
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
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': ix == 0, 'pt-8': ix > 0})} colSpan={2}>
        <Money amount={i.itemTotalExcVat} />
      </td>
    </tr>
    ,
    <tr key={i.productId + '-2'}>
      <td className={classNames('pl-2 pb-2 pr-2 align-top')} colSpan={2}>{i.productName}</td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
        {this.props.currentHouseholdOrder && this.props.currentHouseholdOrder.isOpen &&
          <button className="ml-4 whitespace-no-wrap" onClick={() => this.addToCurrentOrder(i)}><Icon type="add" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Add</button>
        }
      </td>
    </tr>
    ,
    <tr key={i.productId + '-3'}>
      <td className={classNames('pl-2 pr-2')} colSpan={3}>
        <span className="pr-2">
          <ProductFlags p={i} />
        </span>
        <span className="text-grey whitespace-no-wrap">VAT: {i.productVatRate} rate</span>
      </td>
    </tr>
    ]
}