import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem, ProductCatalogueEntry, Household, CollectiveOrder } from '../Types'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface CurrentHouseholdOrderProps { currentHouseholdOrder: HouseholdOrder
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

  render() {
    const householdOrder = this.props.currentHouseholdOrder
    const items = householdOrder.items.filter(i => !i.productDiscontinued)
    const discontinuedItems = householdOrder.items.filter(i => i.productDiscontinued)

    return (
      <table>
        { items.map(this.renderItem) }
        <tr hidden={!discontinuedItems.length}>
          <td colSpan={5} className="text-red font-bold pt-8 pb-2">
            <span className="flex justify-start">
              <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
            </span>
          </td>
        </tr>
        { discontinuedItems.map(this.renderItem) }
        <tr>
          <td className={classNames('pt-8 align-baseline')} colSpan={2}>VAT:</td>
          <td className={classNames('pl-2 pt-8 text-right align-baseline whitespace-no-wrap')} colSpan={3}>
            {householdOrder.oldTotalIncVat !== null && householdOrder.oldTotalExcVat !== null && householdOrder.oldTotalIncVat - householdOrder.oldTotalExcVat != householdOrder.totalIncVat - householdOrder.totalExcVat
              ? <span>
                  <span className="line-through"><Money amount={householdOrder.oldTotalIncVat - householdOrder.oldTotalExcVat} /></span> 
                  <Money className="text-red font-bold" amount={householdOrder.totalIncVat - householdOrder.totalExcVat} />
                </span>
                : <Money amount={householdOrder.totalIncVat - householdOrder.totalExcVat} />
            }
          </td>
        </tr>
        <tr>
          <td className={classNames('pt-2 align-baseline font-bold')} colSpan={2}>Total:</td>
          <td className={classNames('pl-2 pt-2 text-right align-baseline font-bold whitespace-no-wrap')} colSpan={3}>
            {householdOrder.oldTotalIncVat !== null && householdOrder.oldTotalIncVat != householdOrder.totalIncVat
              ? <span>
                  <span className="line-through"><Money amount={householdOrder.oldTotalIncVat} /></span> 
                  <Money className="text-red font-bold" amount={householdOrder.totalIncVat} />
                </span>
                : <Money amount={householdOrder.totalIncVat} />
            }
          </td>
        </tr>
        {/* <div className={classNames('mt-8 flex justify-between items-baseline', {'crossed-out-1': householdOrder.isAbandoned})}> */}
      </table>
    )
  }

  renderItem = (i: OrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top', {'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={`/api/query/product-image/${i.productCode}`} />
      </td>
      <td className={classNames('pb-2 font-bold align-baseline', {'pt-8': ix > 0, '': i.productDiscontinued})}>{i.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-8': ix > 0})}>
        {this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued
          ? <select className="border" value={i.itemQuantity} onChange={e => this.editQuantity(i, parseInt(e.target.value))}>
              {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span className={classNames({'': i.productDiscontinued})}>x {i.itemQuantity}</span>
        }
      </td>
      <td className={classNames('pl-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-8': ix > 0})} colSpan={2}>
        {i.oldItemTotalExcVat !== null && i.oldItemTotalExcVat != i.itemTotalExcVat
          ? <span>
              <span className="line-through"><Money amount={i.oldItemTotalExcVat} /></span> 
              {!i.productDiscontinued && 
                <Money className="text-red font-bold" amount={i.itemTotalExcVat} />
              }
            </span>
          : <Money amount={i.itemTotalExcVat} />
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
        {this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued &&
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