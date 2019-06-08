import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem, ProductCatalogueEntry, Household, CollectiveOrder } from '../Types'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface CurrentHouseholdOrderProps { currentHouseholdOrder: HouseholdOrder
                                            , currentHouseholdOrders: HouseholdOrder[]
                                            , currentOrder: CollectiveOrder | null
                                            , readOnly?: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, {}> {
  removeItem = (item: OrderItem) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, item.productId))
      .then(this.props.reload)
  }

  editQuantity = (item: OrderItem, quantity: number) => {
    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, item.productCode, quantity))
      .then(this.props.reload)
  }

  acceptUpdates = () => {
    if(!this.props.currentHouseholdOrder) return
    this.props.request(ServerApi.command.acceptCatalogueUpdates(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  render() {
    const householdOrder = this.props.currentHouseholdOrder
    const items = householdOrder.items.filter(i => !i.productDiscontinued)
    const discontinuedItems = householdOrder.items.filter(i => i.productDiscontinued)

    return (
      <div>
        {this.renderMessages()}

        <table className="border-collapse w-full">
          { items.map(this.renderItem(householdOrder)) }
          <tr hidden={!discontinuedItems.length}>
            <td colSpan={5} className="text-red font-bold pt-8 pb-2">
              <span className="flex justify-start">
                <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
              </span>
            </td>
          </tr>
          { discontinuedItems.map(this.renderItem(householdOrder)) }
          <tr>
            <td className={classNames('pt-8 align-baseline')} colSpan={5}>
              <div className="flex justify-end">
                <span>VAT:</span>
                <span className={classNames('w-24 text-right align-baseline whitespace-no-wrap')}>
                  {householdOrder.oldTotalIncVat !== null && householdOrder.oldTotalExcVat !== null && householdOrder.oldTotalIncVat - householdOrder.oldTotalExcVat != householdOrder.totalIncVat - householdOrder.totalExcVat?
                    <span>
                      <span className="line-through"><Money amount={householdOrder.oldTotalIncVat - householdOrder.oldTotalExcVat} /></span> 
                      <Money className="text-red font-bold" amount={householdOrder.totalIncVat - householdOrder.totalExcVat} />
                    </span>
                  : <span className={classNames({"line-through text-grey-dark": householdOrder.isAbandoned})}><Money amount={householdOrder.totalIncVat - householdOrder.totalExcVat} /></span>
                  }
                </span>
              </div>
            </td>
          </tr>
          <tr>
            <td className={classNames('pt-4 align-baseline font-bold')} colSpan={5}>
              <div className="flex justify-end">
                <span>Total:</span>
                <span className={classNames('w-24 text-right align-baseline font-bold whitespace-no-wrap')}>
                  {householdOrder.oldTotalIncVat !== null && householdOrder.oldTotalIncVat != householdOrder.totalIncVat?
                    <span>
                      <span className="line-through"><Money amount={householdOrder.oldTotalIncVat} /></span> 
                      <Money className="text-red font-bold" amount={householdOrder.totalIncVat} />
                    </span>
                  : <span className={classNames({"line-through text-grey-dark": householdOrder.isAbandoned})}><Money amount={householdOrder.totalIncVat} /></span>
                  }
                </span>
              </div>
            </td>
          </tr>
        </table>
      </div>
    )
  }

  renderMessages = () => {
    let householdOrder = this.props.currentHouseholdOrder
    if(!householdOrder) return

    const allComplete = this.props.currentHouseholdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    // const householdsInOrder = this.props.households.filter(h => !!this.props.currentHouseholdOrders.find(oh => oh.householdId == h.id))
    // const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)
    const allHouseholdsUpToDate = !!this.props.currentOrder && this.props.currentOrder.allHouseholdsUpToDate;
    const orderMinimumReached = !!this.props.currentOrder && this.props.currentOrder.totalIncVat >= 25000

    if(!!householdOrder.oldTotalExcVat && householdOrder.oldTotalIncVat != householdOrder.totalIncVat)
      return (
        <div className="bg-red-lighter p-2 mb-4"><Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" />The product catalogue was updated and your order has been affected. Please review and accept the changes before continuing.
          <div className="flex justify-end mt-2"><button onClick={e => {e.stopPropagation(); this.acceptUpdates()}}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Accept changes</button></div>
        </div>
      )

    if(!householdOrder.isComplete)
      return

    return (
      <div className="px-2 py-1 bg-blue-lighter text-black flex mb-4">
        <Icon type="info" className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-2" />
        { !allHouseholdsUpToDate?
          <span>Waiting for all households to accept latest catalogue updates</span>
        : !orderMinimumReached?
          <span>Waiting for minimum order to be reached. <br/>Current total is <Money amount={!!this.props.currentOrder && this.props.currentOrder.totalIncVat || 0} /> of &pound;250.00</span>
        : !allComplete?
          <span>Waiting for all orders to be completed</span>
        // : !allPaid?
        //   <span>, waiting for everyone to pay up</span>
        : <span>Collective order can now be placed</span>
        }
      </div>
    )
  }

  renderItem = (householdOrder: HouseholdOrder) => (i: OrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top', {'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${i.productCode}`)} />
      </td>
      <td className={classNames('pb-2 font-bold align-baseline', {'pt-8': ix > 0, '': i.productDiscontinued})}>{i.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-8': ix > 0})}>
        {!this.props.readOnly && this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued
          ? <select className="border" value={i.itemQuantity} onChange={e => this.editQuantity(i, parseInt(e.target.value))}>
              {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span className={classNames({'': i.productDiscontinued})}>x {i.itemQuantity}</span>
        }
      </td>
      <td className={classNames('pl-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-8': ix > 0})} colSpan={2}>
        {i.oldItemTotalExcVat !== null && i.oldItemTotalExcVat != i.itemTotalExcVat?
          <span>
            <span className="line-through"><Money amount={i.oldItemTotalExcVat} /></span> 
            {!i.productDiscontinued && 
              <Money className="text-red font-bold" amount={i.itemTotalExcVat} />
            }
          </span>
        : <span className={classNames({"line-through text-grey-dark": householdOrder.isAbandoned})}><Money amount={i.itemTotalExcVat} /></span>
        }
      </td>
    </tr>
    ,
    <tr key={i.productId + '-2'}>
      <td className={classNames('pb-2 align-top')} colSpan={3}>
        {i.productDiscontinued
          ? <span>
              <span className="">{i.productName}</span><br />
            </span>
          : i.productName
        }
      </td>
      <td className={classNames('pl-2 align-top text-right')}>
        {!this.props.readOnly && this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued &&
          <button className="ml-4" onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
      </td>
    </tr>
    ,
    <tr key={i.productId + '-3'}>
      <td className={classNames('text-grey', {'': i.productDiscontinued})} colSpan={3}>VAT: {i.productVatRate} rate</td>
      <td className={classNames('pl-2')}>&nbsp;</td>
    </tr>
    ]
}