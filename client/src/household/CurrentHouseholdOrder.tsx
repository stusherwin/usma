import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, HouseholdOrderItem } from '../Types'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { ProductFlags } from '../product/ProductList'

export interface CurrentHouseholdOrderProps { currentHouseholdOrder: HouseholdOrder
                                            , readOnly?: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, {}> {
  removeItem = (item: HouseholdOrderItem) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, item.productId))
      .then(this.props.reload)
  }

  editQuantity = (item: HouseholdOrderItem, quantity: number) => {
    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, item.productCode, quantity))
      .then(this.props.reload)
  }

  render() {
    const householdOrder = this.props.currentHouseholdOrder
    const items = householdOrder.items.filter(i => !i.productDiscontinued)
    const discontinuedItems = householdOrder.items.filter(i => i.productDiscontinued)

    return (
      <table className="border-collapse w-full">
        { items.map(this.renderItem(householdOrder)) }
        <tr hidden={!discontinuedItems.length}>
          <td colSpan={5} className={classNames("text-red font-bold pb-2 px-2", {"pt-4": !items.length, "pt-8": items.length})}>
            <span className="flex justify-start">
              <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
            </span>
          </td>
        </tr>
        { discontinuedItems.map(this.renderItem(householdOrder)) }
        <tr>
          <td></td>
          <td className={classNames('pt-8 align-baseline px-2')} colSpan={4}>
            <div className="flex justify-between">
              <span>VAT:</span>
              <span className={classNames('text-right align-baseline whitespace-no-wrap')}>
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
          <td></td>
          <td className={classNames('pt-4 pb-4 px-2 align-baseline font-bold')} colSpan={4}>
            <div className="flex justify-between">
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
    )
  }

  renderItem = (householdOrder: HouseholdOrder) => (i: HouseholdOrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': ix == 0, 'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${i.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0, '': i.productDiscontinued})}>{i.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>
        {!this.props.readOnly && this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued
          ? <select className="border" value={i.itemQuantity} onChange={e => this.editQuantity(i, parseInt(e.target.value))}>
              {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
            </select>
          : <span className={classNames({'': i.productDiscontinued})}>x {i.itemQuantity}</span>
        }
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': ix == 0, 'pt-8': ix > 0})} colSpan={2}>
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
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={3}>
        {i.productDiscontinued
          ? <span>
              <span className="">{i.productName}</span><br />
            </span>
          : i.productName
        }
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
        {!this.props.readOnly && this.props.currentHouseholdOrder.isOpen && !i.productDiscontinued &&
          <button className="ml-4" onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
        }
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