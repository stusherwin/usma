import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, Product, OrderItem, ProductCatalogueEntry } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { AddProduct } from './AddProduct'

export interface CurrentHouseholdOrderProps { householdOrder: HouseholdOrder
                                            , products: ProductCatalogueEntry[]
                                            , loading: boolean
                                            , addingProduct: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            , startAdd: () => void
                                            , cancelAdd: () => void
                                            , confirmAdd: (product: ProductCatalogueEntry) => void
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, {}> {
  removeItem = (item: OrderItem) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productId))
      .then(this.props.reload)
  }

  editQuantity = (item: OrderItem, quantity: number) => {
    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productCode, quantity))
      .then(this.props.reload)
  }

  abandonOrder = () => {
    this.props.request(ServerApi.command.abandonHouseholdOrder(this.props.householdOrder.orderId, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  completeOrder = () => {
    this.props.request(ServerApi.command.completeHouseholdOrder(this.props.householdOrder.orderId, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  reopenOrder = () => {
    this.props.request(ServerApi.command.reopenHouseholdOrder(this.props.householdOrder.orderId, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  leaveOrder = () => {
    this.props.request(ServerApi.command.deleteHouseholdOrder(this.props.householdOrder.orderId, this.props.householdOrder.householdId))
      .then(this.props.reload)
  }

  render() {
    const householdOrder = this.props.householdOrder
    const unusedProducts = this.props.products.filter(p => !householdOrder.items.find(i => i.productCode == p.code))
    const canAddItem = !this.props.addingProduct && householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = !householdOrder.items.length
    const canReopenOrder = !!householdOrder.items.length && !householdOrder.isOpen
    const canAbandonOrder = !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canAbandonOrder, canCompleteOrder, canAddItem]

    return (
      <div>
        {orderButtons.some(b => b) &&
          <div className="bg-order-dark p-2 pt-0 flex flex-wrap content-start items-start">
            {canLeaveOrder && 
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.leaveOrder}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</button>
            }
            {canReopenOrder &&
              <div className="flex-no-grow flex-no-shrink mr-2 mt-2">
                <span className="mr-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Order was {householdOrder.isAbandoned ? 'abandoned' : 'completed'}</span>
                <button disabled={this.props.addingProduct} onClick={this.reopenOrder}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen</button>
              </div>
            }
            {canAbandonOrder &&
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.abandonOrder}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon</button>
            }
            {canCompleteOrder && 
              <button className="flex-no-grow flex-no-shrink mr-2 mt-2" disabled={this.props.addingProduct} onClick={this.completeOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete</button>
            }
            {canAddItem &&
              <button className="flex-no-grow flex-no-shrink ml-auto mt-2" onClick={this.props.startAdd}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add item</button>
            }
          </div>
        }
        {!this.props.addingProduct && 
          <div className="shadow-inner-top bg-white px-2 py-4">
            {!householdOrder.items.length &&
              <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products added to this order yet {!this.props.products.length && ' - the product catalogue is empty'}</div>
            }
            {!!householdOrder.items.length &&
              <div>
                { householdOrder.items.map((i, ix) =>
                  <div key={i.productId} className={classNames({'mt-8': ix > 0})}>
                    <div className={classNames('flex justify-between items-baseline', {'crossed-out-1': householdOrder.isAbandoned})}>
                      <span className="flex-no-shrink flex-no-grow font-bold w-1/3">{i.productCode}</span>
                      <span className="flex-no-shrink flex-no-grow w-1/3 text-center">
                        {householdOrder.isOpen
                        ? <select className="flex-no-grow flex-no-shrink mr-2 border" value={i.itemQuantity} onChange={e => this.editQuantity(i, parseInt(e.target.value))}>
                            {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                          </select>
                        : <span>x {i.itemQuantity}</span>
                        }
                      </span>
                      <Money className="flex-no-shrink flex-no-grow w-1/3 text-right" amount={i.itemTotalExcVat} />
                    </div>
                    <div className="flex justify-between items-end">
                      <span className={classNames('flex-no-grow', {'crossed-out-1': householdOrder.isAbandoned})}>{i.productName}</span>
                      {householdOrder.isOpen &&
                        <button className="ml-4" disabled={this.props.addingProduct} onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                      }
                    </div>
                  </div>
                )}
                <div className={classNames('mt-8 flex justify-between items-baseline', {'crossed-out-1': householdOrder.isAbandoned})}>
                  <span className="flex-no-shrink flex-no-grow">VAT:</span>
                  <Money className="flex-no-shrink flex-no-grow text-right" amount={householdOrder.totalIncVat - householdOrder.totalExcVat} />
                </div>
                <div className={classNames('mt-2 flex justify-between items-baseline', {'crossed-out-1': householdOrder.isAbandoned})}>
                  <span className="flex-no-shrink flex-no-grow font-bold">Total:</span>
                  <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={householdOrder.totalIncVat} />
                </div>
              </div>
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