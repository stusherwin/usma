import * as React from 'react';

import { HouseholdOrder, Product, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'

export interface HouseholdOrderPageProps { householdOrder: HouseholdOrder
                                         , products: Product[]
                                         , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                         , reload: () => Promise<void>
                                         , referrer: 'order' | 'household'
                                         }

export interface HouseholdOrderPageState { addingProduct: Product | null
                                         , addingProductQuantity: number
                                         , editingProduct: Product | null
                                         , editingProductQuantity: number
                                         }

export class HouseholdOrderPage extends React.Component<HouseholdOrderPageProps, HouseholdOrderPageState> {
  constructor(props: HouseholdOrderPageProps) {
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

  render() {
    const householdOrder = this.props.householdOrder
    const unusedProducts = this.props.products.filter(p => !householdOrder.items.find(i => i.productId == p.id))

    return (
      <div>
        <div className="bg-img-household-order bg-no-repeat bg-16 pl-16 min-h-16">
          {this.props.referrer == 'order'
          ? (
            <div>
              <div>
                <RouterLink path="/orders">Orders</RouterLink> &gt;
                {Util.formatDate(householdOrder.orderCreatedDate)} &gt;
              </div>
              <h1>{householdOrder.householdName}</h1>
              <div>Status: {householdOrder.status}</div>
              <RouterLink path={`/households/${householdOrder.householdId}`}>View household</RouterLink>
            </div>
          )
          : (
            <div>
              <div>
                <RouterLink path="/households">Households</RouterLink> &gt;
                <RouterLink path={`/households/${householdOrder.householdId}`}>{householdOrder.householdName}</RouterLink> &gt;
              </div>
              <h1>{Util.formatDate(householdOrder.orderCreatedDate)}</h1>
              <div>Status: {householdOrder.status}</div>
            </div>
          )}
          {householdOrder.canBeAmended && !householdOrder.isOpen &&
            <Button disabled={!!this.state.addingProduct} action={this.reopenOrder}>Reopen order</Button>
          }
          {householdOrder.canBeAmended && householdOrder.isOpen && (
            <span>
              <Button disabled={!!this.state.addingProduct} action={this.cancelOrder}>Cancel order</Button>
              <Button disabled={!!this.state.addingProduct} action={this.completeOrder}>Complete order</Button>
            </span>
          )}
        </div>
        <h2>Items</h2>
        <div>
          {!householdOrder.items.length &&
            <div>No items added to this order</div>
          }
          {householdOrder.canBeAmended && householdOrder.isOpen && !!unusedProducts.length && !this.state.addingProduct &&
            <div>
              <Button action={() => this.startAdd(unusedProducts[0])}>Add item</Button>
            </div>
          }
          {this.state.addingProduct &&
            <div>
              <span>
                <select value={this.state.addingProduct.id} onChange={this.addingProductChanged}>
                  {unusedProducts.map(p => <option key={p.id} value={p.id}>{p.name}</option>)}
                </select>
              </span>
              <span>
                <select value={this.state.addingProductQuantity} onChange={this.addingQuantityChanged}>
                  {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                </select>
              </span>
              <Money amount={this.state.addingProduct.price * this.state.addingProductQuantity} />
              <Button action={this.confirmAdd}>Save</Button>
              <Button action={this.cancelAdd}>Cancel</Button>
            </div>
          }
          {householdOrder.items.map(i => this.state.editingProduct && this.state.editingProduct.id == i.productId 
          ? (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>
                <select value={this.state.editingProductQuantity} onChange={this.editingQuantityChanged}>
                  {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                </select>
              </span>
              <Money amount={this.state.editingProduct.price * this.state.editingProductQuantity} />
              <Button action={this.confirmEdit}>Save</Button>
              <Button action={this.cancelEdit}>Cancel</Button>
            </div>
          )
          : (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>x {i.itemQuantity}</span>
              <Money amount={i.itemTotal} />
              {householdOrder.canBeAmended && householdOrder.isOpen &&
                <span>
                  <Button disabled={!!this.state.addingProduct} action={() => this.startEdit(i)}>Edit</Button>
                  <Button disabled={!!this.state.addingProduct} action={() => this.removeItem(i)}>Remove</Button>
                </span>
              }
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={householdOrder.total} />
          </div>
        </div>
      </div>
    )
  }
}