import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, Product, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Icon } from './Icon'
import { Money } from './Money'

export interface CurrentHouseholdOrderProps { householdOrder: HouseholdOrder
                                            , products: Product[]
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => Promise<void>
                                            }

export interface CurrentHouseholdOrderState { addingProduct: Product | null
                                            , addingProductQuantity: number
                                            , editingProduct: Product | null
                                            , editingProductQuantity: number
                                            }

export class CurrentHouseholdOrder extends React.Component<CurrentHouseholdOrderProps, CurrentHouseholdOrderState> {
  constructor(props: CurrentHouseholdOrderProps) {
    super(props)

    this.state = { addingProduct: null
                 , addingProductQuantity: 1
                 , editingProduct: null
                 , editingProductQuantity: 1
                 }
  }

  startAdd = (product: Product) => this.setState({ addingProduct: product })

  addingProductChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingProduct: this.props.products.find(p => '' + p.id == event.target.value) || null })

  addingQuantityChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingProductQuantity: parseInt(event.target.value) || 1
                  })

  cancelAdd = () =>
    this.setState({ addingProduct: null
                  , addingProductQuantity: 1
                  })

  confirmAdd = () => {
    if(!this.state.addingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, this.state.addingProduct.id, this.state.addingProductQuantity))
      .then(this.props.reload)
      .then(_ => this.setState({ addingProduct: null
                               , addingProductQuantity: 1
                               }))
  }

  removeItem = (item: OrderItem) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, item.productId))
      .then(this.props.reload)
  }

  startEdit = (item: OrderItem) => {
    const product = this.props.products.find(p => p.id == item.productId)
    if(!product) return

    this.setState({ editingProduct: product
                  , editingProductQuantity: item.itemQuantity
                  })
  } 

  editingQuantityChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ editingProductQuantity: parseInt(event.target.value) || 1
                  })
                  
  confirmEdit = () => {
    if(!this.state.editingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.householdOrder.orderId, this.props.householdOrder.householdId, this.state.editingProduct.id, this.state.editingProductQuantity))
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
    const unusedProducts = this.props.products.filter(p => !householdOrder.items.find(i => i.productId == p.id))
    const canAddItem = householdOrder.canBeAmended && householdOrder.isOpen && !!unusedProducts.length
    const canLeaveOrder = householdOrder.canBeAmended && !householdOrder.items.length
    const canReopenOrder = householdOrder.canBeAmended && !!householdOrder.items.length && !householdOrder.isOpen
    const canCancelOrder = householdOrder.canBeAmended && !!householdOrder.items.length && householdOrder.isOpen
    const canCompleteOrder = householdOrder.canBeAmended && !!householdOrder.items.length && householdOrder.isOpen
    const orderButtons = [canLeaveOrder, canReopenOrder, canCancelOrder, canCompleteOrder]
    const addItemButton = canAddItem && !this.state.addingProduct

    return (
      <div>
        {(addItemButton || orderButtons.some(b => b)) && 
          <div className={classNames('bg-order-dark p-2 pt-0', {'flex justify-between': orderButtons.filter(b => b).length == 1})}>
            {orderButtons.some(b => b) && 
              <div>
                {canLeaveOrder && 
                  <Button className="mr-2 mt-2" disabled={!!this.state.addingProduct} action={this.leaveOrder}><Icon type="leave" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Leave order</Button>
                }
                {canReopenOrder &&
                  <Button className="mr-2 mt-2" disabled={!!this.state.addingProduct} action={this.reopenOrder}><Icon type="undo" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Reopen order</Button>
                }
                {canCancelOrder &&
                  <Button className="mr-2 mt-2" disabled={!!this.state.addingProduct} action={this.cancelOrder}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Cancel order</Button>
                }
                {canCompleteOrder && 
                  <Button className="mr-2 mt-2" disabled={!!this.state.addingProduct} action={this.completeOrder}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Complete order</Button>
                }
              </div>
            }
            {addItemButton &&
              <div className={classNames({'mt-2': orderButtons.filter(b => b).length == 1, 'flex justify-end mt-4': orderButtons.filter(b => b).length > 1})}>
                <Button action={() => this.startAdd(unusedProducts[0])}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Add order item</Button>
              </div>
            }
          </div>
        }
        <div>
          {!householdOrder.items.length && !this.state.addingProduct &&
            <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No items added yet</div>
          }
          {this.state.addingProduct &&
            <div className="bg-product-lightest p-2">
              <h3 className="mb-4">Add order item</h3>
              <div className="flex justify-between items-baseline mb-4">
                <select className="flex-grow mr-2" value={this.state.addingProduct.id} onChange={this.addingProductChanged}>
                  {unusedProducts.map(p => <option key={p.id} value={p.id}>{p.code}: {p.name}</option>)}
                </select>
                <select className="flex-no-grow flex-no-shrink mr-2" value={this.state.addingProductQuantity} onChange={this.addingQuantityChanged}>
                  {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                </select>
                <Money className="flex-no-grow flex-no-shrink" amount={this.state.addingProduct.price * this.state.addingProductQuantity} />
              </div>
              <div className="flex justify-end items-baseline">
                <Button className="ml-2" action={this.confirmAdd}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</Button>
                <Button className="ml-2" action={this.cancelAdd}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</Button>
              </div>
            </div>
          }
          {!!householdOrder.items.length &&
            <table className="border-collapse w-full mb-4">
              {householdOrder.items.map(i => this.state.editingProduct && this.state.editingProduct.id == i.productId 
              ? (
                <tr key={i.productId}>
                  <td colSpan={4} className="bg-product-lightest p-2">
                    <h3 className="mb-4">Edit order item</h3>
                    <div className="flex justify-between items-baseline mb-4">
                      <span className="flex-grow mr-2">{i.productCode}: {i.productName}</span>
                      <select className="flex-no-grow flex-no-shrink mr-2" value={this.state.editingProductQuantity} onChange={this.editingQuantityChanged}>
                        {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                      </select>
                      <Money className="flex-no-grow flex-no-shrink" amount={this.state.editingProduct.price * this.state.editingProductQuantity} />
                    </div>
                    <div className="flex justify-end items-baseline">
                      <Button className="ml-2" action={this.confirmEdit}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</Button>
                      <Button className="ml-2" action={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Cancel</Button>
                    </div>
                  </td>
                </tr>
              )
              : (
                <tr key={i.productId}>
                  <td className="pt-2 pl-2 pr-2 w-full">{i.productCode}: {i.productName}</td>
                  <td className="pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={i.itemTotal} /></td>
                  <td className="pt-2 pr-2 w-1">
                    {householdOrder.canBeAmended && householdOrder.isOpen &&
                      <span className="whitespace-no-wrap">
                        <Button disabled={!!this.state.addingProduct} action={() => this.startEdit(i)}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></Button>
                        <Button className="ml-2" disabled={!!this.state.addingProduct} action={() => this.removeItem(i)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></Button>
                      </span>
                    }
                  </td>
                </tr>
              ))}
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={householdOrder.total} /></td>
                <td className="pt-2 pr-2"></td>
              </tr>
            </table>
          }
        </div>
      </div>
    )
  }
}