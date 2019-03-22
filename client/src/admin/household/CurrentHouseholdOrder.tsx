import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, Product, OrderItem, ProductCatalogueEntry } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { AddProduct } from '../product/AddProduct'

export interface CurrentHouseholdOrderProps { householdOrder: HouseholdOrder
                                            , products: ProductCatalogueEntry[]
                                            , loading: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            }

export interface CurrentHouseholdOrderState { addingProduct: boolean
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, CurrentHouseholdOrderState> {
  constructor(props: CurrentHouseholdOrderProps) {
    super(props)

    this.state = { addingProduct: false
                 }
  }

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () =>
    this.setState({ addingProduct: false
                  })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if(!this.state.addingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, product.code, 1))
      .then(this.props.reload)
      .then(_ => this.setState({ addingProduct: false
                               }))
  }

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
    const canAddItem = householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = !householdOrder.items.length
    const canReopenOrder = !!householdOrder.items.length && !householdOrder.isOpen
    const canAbandonOrder = !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canAbandonOrder, canCompleteOrder]

    return (
      <div>
        {(orderButtons.some(b => b)) && 
          <div className="bg-order-dark p-2 pt-0">
            {canLeaveOrder && 
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.leaveOrder}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</button>
            }
            {canReopenOrder &&
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.reopenOrder}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</button>
            }
            {canAbandonOrder &&
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.abandonOrder}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Abandon order</button>
            }
            {canCompleteOrder && 
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.completeOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete order</button>
            }
          </div>
        }
        <div className="mb-4">
          {canAddItem &&
            <div className="p-2 flex justify-end">
              <button onClick={this.startAdd}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add a product</button>
            </div>
          }
          {!householdOrder.items.length &&
            <div className="text-grey-darker p-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products added to this order yet</div>
          }
          {!!householdOrder.items.length &&
            <div>
              { householdOrder.items.map((i, ix) =>
                <div key={i.productId} className="p-2 mb-4 mt-4">
                  <div className="flex justify-between items-baseline">
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
                  <div className="flex justify-between items-end mt-2">
                    <span className="flex-no-grow">{i.productName}</span>
                    {householdOrder.isOpen &&
                      <button className="ml-4" disabled={this.state.addingProduct} onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                    }
                  </div>
                </div>
              )}
              <div className="p-2 flex justify-between items-baseline">
                <span className="flex-no-shrink flex-no-grow font-bold">Total</span>
                <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={householdOrder.totalIncVat} />
              </div>
            </div>
          }
          {canAddItem && householdOrder.items.length > 3 &&
            <div className="p-2 flex justify-end">
              <button onClick={this.startAdd}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add a product</button>
            </div>
          }
        </div>
        {this.state.addingProduct &&
          <AddProduct products={unusedProducts}
                      loading={this.props.loading}
                      cancelAdd={this.cancelAdd}
                      confirmAdd={this.confirmAdd} />
        }
      </div>
    )
  }
}