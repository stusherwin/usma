import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, Product, OrderItem, ProductCatalogueEntry } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Icon } from './Icon'
import { Money } from './Money'
import { AddProduct } from './AddProduct'

export interface CurrentHouseholdOrderProps { householdOrder: HouseholdOrder
                                            , products: ProductCatalogueEntry[]
                                            , loading: boolean
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            }

export interface CurrentHouseholdOrderState { addingProduct: boolean
                                            , editingProduct: string | null
                                            , editingProductQuantity: number
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, CurrentHouseholdOrderState> {
  constructor(props: CurrentHouseholdOrderProps) {
    super(props)

    this.state = { addingProduct: false
                 , editingProduct: null
                 , editingProductQuantity: 1
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

  startEdit = (item: OrderItem) => {
    this.setState({ editingProduct: item.productCode
                  , editingProductQuantity: item.itemQuantity
                  })
  } 

  editingQuantityChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ editingProductQuantity: parseInt(event.target.value) || 1
                  })
                  
  confirmEdit = () => {
    if(!this.state.editingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, this.state.editingProduct, this.state.editingProductQuantity))
      .then(this.props.reload)
      .then(_ => this.setState({ editingProduct: null
                               , editingProductQuantity: 1
                               }))
  }

  cancelEdit = () =>
    this.setState({ editingProduct: null
                  , editingProductQuantity: 1
                  })

  cancelOrder = () => {
    this.props.request(ServerApi.command.cancelHouseholdOrder(this.props.householdOrder.orderId, this.props.householdOrder.householdId))
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
    const canAddItem = householdOrder.canBeAmended && householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = householdOrder.canBeAmended && !householdOrder.items.length
    const canReopenOrder = householdOrder.canBeAmended && !!householdOrder.items.length && !householdOrder.isOpen
    const canCancelOrder = householdOrder.canBeAmended && !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = householdOrder.canBeAmended && !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canCancelOrder, canCompleteOrder]

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
            {canCancelOrder &&
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.cancelOrder}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Cancel order</button>
            }
            {canCompleteOrder && 
              <button className="mr-2 mt-2" disabled={this.state.addingProduct} onClick={this.completeOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete order</button>
            }
          </div>
        }
        <div className="mb-4">
          {!this.state.addingProduct && !householdOrder.items.length &&
            <div className="text-grey-darker p-2"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products added to this order yet</div>
          }
          {!this.state.addingProduct && canAddItem &&
            <div className="p-2 flex justify-end">
              <button onClick={this.startAdd}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add a product</button>
            </div>
          }
          {!!householdOrder.items.length &&
            <div>
              {householdOrder.items.map((i, ix) => this.state.editingProduct == i.productCode
              ? (
                <div key={i.productId} className="bg-product-lightest p-2">
                  <h3 className="mb-4">Edit order item</h3>
                  <div className="flex justify-between items-baseline mb-4">
                    <span className="flex-grow mr-2">{i.productCode}: {i.productName}</span>
                    <select className="flex-no-grow flex-no-shrink mr-2" value={this.state.editingProductQuantity} onChange={this.editingQuantityChanged}>
                      {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                    </select>
                    <Money className="flex-no-grow flex-no-shrink" amount={i.productPrice * this.state.editingProductQuantity} />
                  </div>
                  <div className="flex justify-end items-baseline">
                    <button className="ml-2" onClick={this.confirmEdit}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
                    <button className="ml-2" onClick={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Cancel</button>
                  </div>
                </div>
              )
              : (
                <div key={i.productId} className="p-2 mb-4 mt-4">
                  <div className="flex justify-between items-baseline">
                    <span className="flex-no-shrink flex-no-grow w-1/3">{i.productCode}</span>
                    <span className="flex-no-shrink flex-no-grow w-1/3">x {i.itemQuantity}</span>
                    <Money className="flex-no-shrink flex-no-grow text-right w-1/3" amount={i.itemTotal} />
                  </div>
                  <p className="mt-2">{i.productName}</p>
                  <div className="flex justify-between items-end mt-2">
                    <span className="flex-no-shrink flex-no-grow text-grey">VAT: {i.productVatRate} rate</span>
                    {householdOrder.canBeAmended && householdOrder.isOpen &&
                      <span className="whitespace-no-wrap">
                        <button disabled={this.state.addingProduct} onClick={() => this.startEdit(i)}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></button>
                        <button className="ml-2" disabled={this.state.addingProduct} onClick={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                      </span>
                    }
                  </div>
                </div>
              ))}
              <div className="p-2 flex justify-between items-baseline">
                <span className="flex-no-shrink flex-no-grow font-bold">Total</span>
                <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={householdOrder.total} />
              </div>
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