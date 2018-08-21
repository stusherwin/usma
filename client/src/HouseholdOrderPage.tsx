import * as React from 'react';

import { HouseholdOrder, Product, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface HouseholdOrderPageProps { order: HouseholdOrder
                                         , products: Product[]
                                         , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                         , navigate: (location: string) => void
                                         , reload: () => void
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

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.order.orderId, this.props.order.householdId, this.state.addingProduct.id, this.state.addingProductQuantity))
      .then(_ => {
        this.setState({ addingProduct: null
                      , addingProductQuantity: 1
                      })
        this.props.reload()
      })
  }

  removeItem = (item: OrderItem) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.order.orderId, this.props.order.householdId, item.productId))
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

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.order.orderId, this.props.order.householdId, this.state.editingProduct.id, this.state.editingProductQuantity))
      .then(_ => {
        this.setState({ editingProduct: null
                      , editingProductQuantity: 1
                      })
        this.props.reload()
      })
  }

  cancelEdit = () =>
    this.setState({ editingProduct: null
                  , editingProductQuantity: 1
                  })

  cancelOrder = () => {
    this.props.request(ServerApi.command.cancelHouseholdOrder(this.props.order.orderId, this.props.order.householdId))
      .then(this.props.reload)
  }

  uncancelOrder = () => {
    this.props.request(ServerApi.command.uncancelHouseholdOrder(this.props.order.orderId, this.props.order.householdId))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const unusedProducts = this.props.products.filter(p => !order.items.find(i => i.productId == p.id))

    return (
      <div>
        <div>
          <Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;
          <Link action={_ => this.props.navigate('/orders/' + order.orderId)}>{Util.formatDate(order.orderCreatedDate)}</Link> &gt;
        </div>
        <h1>{order.householdName} {order.cancelled && ' (cancelled)'}</h1>
        <div>
          {!order.orderComplete && (
            order.cancelled
              ? <Link disabled={!!this.state.addingProduct} action={this.uncancelOrder}>Uncancel</Link>
              : <Link disabled={!!this.state.addingProduct} action={this.cancelOrder}>Cancel order</Link>
          )}
          {!order.orderComplete && !order.cancelled && 
            <Link disabled={!!this.state.addingProduct} action={_ => {}}>Record payment</Link>
          }
        </div>
        <div>
        {!order.orderComplete && !order.cancelled && !!unusedProducts.length &&
          <Link action={() => this.startAdd(unusedProducts[0])}>Add</Link>
        }
        </div>
        <div>
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
              <Link action={this.confirmAdd}>Add</Link>
              <Link action={this.cancelAdd}>Cancel</Link>
            </div>
          }
          {order.items.map(i => this.state.editingProduct && this.state.editingProduct.id == i.productId 
          ? (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>
                <select value={this.state.editingProductQuantity} onChange={this.editingQuantityChanged}>
                  {[1,2,3,4,5,6,7,8,9,10].map(q => <option key={q} value={q}>x {q}</option>)}
                </select>
              </span>
              <Money amount={this.state.editingProduct.price * this.state.editingProductQuantity} />
              <Link action={this.confirmEdit}>Save</Link>
              <Link action={this.cancelEdit}>Cancel</Link>
            </div>
          )
          : (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>x {i.itemQuantity}</span>
              <Money amount={i.itemTotal} />
              {!order.orderComplete && !order.cancelled &&
                <span>
                  <Link disabled={!!this.state.addingProduct} action={() => this.startEdit(i)}>Edit</Link>
                  <Link disabled={!!this.state.addingProduct} action={() => this.removeItem(i)}>Remove</Link>
                </span>
              }
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={order.total} />
          </div>
        </div>
      </div>
    )
  }
}