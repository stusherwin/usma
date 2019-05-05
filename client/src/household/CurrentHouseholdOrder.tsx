import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem, ProductCatalogueEntry, Household, CollectiveOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { AddProduct } from './AddProduct'

export interface CurrentHouseholdOrderProps { currentOrder: CollectiveOrder
                                            , currentHouseholdOrder: HouseholdOrder
                                            , currentHouseholdOrders: HouseholdOrder[]
                                            , products: ProductCatalogueEntry[]
                                            , households: Household[]
                                            , loading: boolean
                                            , addingProduct: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            , startAdd: () => void
                                            , cancelAdd: () => void
                                            , confirmAdd: (product: ProductCatalogueEntry) => Promise<void>
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

  abandonOrder = () => {
    this.props.request(ServerApi.command.abandonHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  completeOrder = () => {
    this.props.request(ServerApi.command.completeHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  reopenOrder = () => {
    this.props.request(ServerApi.command.reopenHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  leaveOrder = () => {
    this.props.request(ServerApi.command.deleteHouseholdOrder(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  acceptUpdates = () => {
    this.props.request(ServerApi.command.acceptCatalogueUpdates(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId))
      .then(this.props.reload)
  }

  render() {
    const householdOrder = this.props.currentHouseholdOrder
    const unusedProducts = this.props.products.filter(p => !householdOrder.items.find(i => i.productCode == p.code))
    const canAddItem = !this.props.addingProduct && householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = !householdOrder.items.length
    const canReopenOrder = !!householdOrder.items.length && !householdOrder.isOpen
    const canAbandonOrder = !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

    const allComplete = this.props.currentHouseholdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const householdsInOrder = this.props.households.filter(h => !!this.props.currentHouseholdOrders.find(oh => oh.householdId == h.id))
    const allPaid = householdsInOrder.reduce((paid, h) => paid && h.balance > 0, true)
    const orderMinimumReached = this.props.currentOrder.totalIncVat >= 25000

    const items = householdOrder.items.filter(i => !i.productDiscontinued)
    const discontinuedItems = householdOrder.items.filter(i => i.productDiscontinued)

    const renderItem = (i: OrderItem, ix: number) => 
      [
      <tr key={i.productId + '-1'}>
        <td className={classNames('w-20 h-20 align-top', {'pt-8': ix > 0})} rowSpan={3}>
          <img className="w-20 h-20 -ml-1" src={`/api/query/product-image/${i.productCode}`} />
        </td>
        <td className={classNames('pb-2 font-bold align-baseline', {'pt-8': ix > 0, '': i.productDiscontinued})}>{i.productCode}</td>
        <td className={classNames('pl-2 pb-2 align-baseline', {'pt-8': ix > 0})}>
          {householdOrder.isOpen && !i.productDiscontinued
            ? <select className="border" value={i.itemQuantity} onChange={e => this.editQuantity(i, parseInt(e.target.value))}>
                {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
              </select>
            : <span className={classNames({'': i.productDiscontinued})}>x {i.itemQuantity}</span>
          }
        </td>
        <td className={classNames('pl-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-8': ix > 0})} colSpan={2}>
          {i.newItemTotalExcVat !== null && i.newItemTotalExcVat != i.itemTotalExcVat
            ? <span>
                <span className="line-through"><Money amount={i.itemTotalExcVat} /></span> 
                {!i.productDiscontinued && 
                  <Money className="text-red font-bold" amount={i.newItemTotalExcVat} />
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
          {householdOrder.isOpen && !i.productDiscontinued &&
            <button className="ml-4" disabled={this.props.addingProduct} onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
          }
        </td>
      </tr>
      ,
      <tr key={i.productId + '-3'}>
        <td className={classNames('text-grey', {'': i.productDiscontinued})} colSpan={3}>VAT: {i.productVatRate} rate</td>
        <td className={classNames('pl-2')}>&nbsp;</td>
      </tr>
      ]

    return (
      <div>
        {orderButtons.some(b => b) &&
          <div className="bg-order-dark p-2 pt-0 flex flex-wrap content-start items-start">
            {canLeaveOrder && 
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.leaveOrder}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</button>
            }
            {canReopenOrder &&
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.reopenOrder}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</button>
            }
            {canAbandonOrder &&
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.abandonOrder}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon</button>
            }
            {canCompleteOrder && 
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.completeOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete</button>
            }
            {canAddItem &&
              <button className="flex-no-grow flex-no-shrink ml-auto mt-2" onClick={this.props.startAdd}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add items</button>
            }
          </div>
        }
        {!this.props.addingProduct && 
          <div className="shadow-inner-top bg-white px-2 py-4">
            {!householdOrder.items.length &&
              <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products added to this order yet {!this.props.products.length && ' - the product catalogue is empty'}</div>
            }
            {householdOrder.isComplete &&
              <div className="bg-blue-lighter p-2 mb-4"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Order complete
              {!orderMinimumReached?
                <span>, waiting for minimum order to be reached. Current total is <Money amount={this.props.currentOrder.totalIncVat} /> of &pound;250.00</span>
              : !allComplete?
                <span>, waiting for all orders to be completed</span>
              : !allPaid?
                <span>, waiting for everyone to pay up</span>
              : <span>, collective order can now be placed</span>
              }
              </div>
            }
            {householdOrder.isAbandoned &&
              <div className="bg-blue-lighter p-2 mb-4"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Order was abandoned</div>
            }
            {!!householdOrder.newTotalExcVat && householdOrder.newTotalIncVat != householdOrder.totalIncVat &&
              <div className="bg-red-lighter p-2 mb-4"><Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" />The product catalogue was updated and your order has been affected. Please review and accept the changes before continuing.
                <div className="flex justify-end mt-2"><button onClick={this.acceptUpdates}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Accept changes</button></div>
              </div>
            }
            {!!householdOrder.items.length &&
              <table>
                { items.map(renderItem) }
                <tr hidden={!discontinuedItems.length}>
                  <td colSpan={5} className="text-red font-bold pt-8 pb-2">
                    <span className="flex justify-start">
                      <Icon type="alert" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><span>The following products were discontinued <br />and will be removed:</span>
                    </span>
                  </td>
                </tr>
                { discontinuedItems.map(renderItem) }
                <tr>
                  <td className={classNames('pt-8 align-baseline')} colSpan={2}>VAT:</td>
                  <td className={classNames('pl-2 pt-8 text-right align-baseline whitespace-no-wrap')} colSpan={3}>
                    {householdOrder.newTotalIncVat !== null && householdOrder.newTotalExcVat !== null && householdOrder.newTotalIncVat != householdOrder.totalIncVat
                      ? <span>
                          <span className="line-through"><Money amount={householdOrder.totalIncVat - householdOrder.totalExcVat} /></span> 
                          <Money className="text-red font-bold" amount={householdOrder.newTotalIncVat - householdOrder.newTotalExcVat} />
                        </span>
                        : <Money amount={householdOrder.totalIncVat - householdOrder.totalExcVat} />
                    }
                  </td>
                </tr>
                <tr>
                  <td className={classNames('pt-2 align-baseline font-bold')} colSpan={2}>Total:</td>
                  <td className={classNames('pl-2 pt-2 text-right align-baseline font-bold whitespace-no-wrap')} colSpan={3}>
                    {householdOrder.newTotalIncVat !== null && householdOrder.newTotalIncVat != householdOrder.totalIncVat
                      ? <span>
                          <span className="line-through"><Money amount={householdOrder.totalIncVat} /></span> 
                          <Money className="text-red font-bold" amount={householdOrder.newTotalIncVat} />
                        </span>
                        : <Money amount={householdOrder.totalIncVat} />
                    }
                  </td>
                </tr>
                {/* <div className={classNames('mt-8 flex justify-between items-baseline', {'crossed-out-1': householdOrder.isAbandoned})}> */}
              </table>
            }
          </div>
        }
        {this.props.addingProduct &&
          <AddProduct products={unusedProducts}
                      loading={this.props.loading}
                      cancelAdd={this.props.cancelAdd}
                      confirmAdd={this.props.confirmAdd} />
        }
      </div>
    )
  }
}