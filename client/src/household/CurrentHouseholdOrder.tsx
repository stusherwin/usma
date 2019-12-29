import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, HouseholdOrderItem } from '../util/Types'
import { ServerApi } from '../util/ServerApi'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'

import { CurrentHouseholdOrderItem } from './CurrentHouseholdOrderItem'

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
        { items.map((item, index) => 
          <CurrentHouseholdOrderItem householdOrder={householdOrder} 
                                     item={item} 
                                     index={index} 
                                     editQuantity={this.editQuantity}
                                     removeItem={this.removeItem} />
        )}
        <tr hidden={!discontinuedItems.length}>
          <td colSpan={5} className={classNames("text-red font-bold pb-2 px-2", {"pt-4": !items.length, "pt-8": items.length})}>
            <span className="flex justify-start">
              <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
            </span>
          </td>
        </tr>
        { discontinuedItems.map((item, index) => 
          <CurrentHouseholdOrderItem householdOrder={householdOrder}
                                     item={item} 
                                     index={index} 
                                     editQuantity={this.editQuantity}
                                     readOnly={this.props.readOnly}
                                     removeItem={this.removeItem} />
        )}
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
}