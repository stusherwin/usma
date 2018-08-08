import * as React from 'react';

import { HouseholdOrderSummary, Product, OrderSummary_Item } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface HouseholdOrderPageProps { orderId: number
                                         , householdId: number
                                         , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                         , navigate: (location: string) => void
                                         }

export interface HouseholdOrderPageState { summary: HouseholdOrderSummary | null
                                         , products: Product[]
                                         , initialised: boolean
                                         , addingProduct: Product | null
                                         , addingProductQuantity: number
                                         , editingProduct: Product | null
                                         , editingProductQuantity: number
                                         }

export class HouseholdOrderPage extends React.Component<HouseholdOrderPageProps, HouseholdOrderPageState> {
  constructor(props: HouseholdOrderPageProps) {
    super(props)

    this.state = { summary: null
                 , products: []
                 , initialised: false
                 , addingProduct: null
                 , addingProductQuantity: 1
                 , editingProduct: null
                 , editingProductQuantity: 1
                 }
  }

  componentDidMount() {
    this.props.request(Promise.all([ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId), ServerApi.query.products()]))
      .then(results => this.setState({ summary: results[0]
                                     , products: results[1]
                                     , initialised: true
                                     }))
      .catch(_ => this.setState({ initialised: true }))
  }

  startAdd = (product: Product) => this.setState({ addingProduct: product })

  addingProductChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingProduct: this.state.products.find(p => '' + p.id == event.target.value) || null })

  addingQuantityChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ addingProductQuantity: parseInt(event.target.value) || 1
                  })

  cancelAdd = () =>
    this.setState({ addingProduct: null
                  , addingProductQuantity: 1
                  })

  confirmAdd = () => {
    if(!this.state.addingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.orderId, this.props.householdId, this.state.addingProduct.id, this.state.addingProductQuantity))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(summary => this.setState({ summary
                                     , addingProduct: null
                                     , addingProductQuantity: 1
                                     }))
  }

  delete = (item: OrderSummary_Item) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.orderId, this.props.householdId, item.productId))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(summary => this.setState({ summary
                                     }))
  }

  startEdit = (item: OrderSummary_Item) => {
    let product = this.state.products.find(p => p.id == item.productId)
    if(!product) return

    this.setState({ editingProduct: product
                  , editingProductQuantity: item.quantity
                  })
  } 

  editingQuantityChanged = (event: React.ChangeEvent<HTMLSelectElement>) =>
    this.setState({ editingProductQuantity: parseInt(event.target.value) || 1
                  })
                  
  confirmEdit = () => {
    if(!this.state.editingProduct) return

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.orderId, this.props.householdId, this.state.editingProduct.id, this.state.editingProductQuantity))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(summary => this.setState({ summary
                                     , editingProduct: null
                                     , editingProductQuantity: 1
                                     }))
  }

  cancelEdit = () =>
    this.setState({ editingProduct: null
                  , editingProductQuantity: 1
                  })

  cancelOrder = () => {
    this.props.request(ServerApi.command.cancelHouseholdOrder(this.props.orderId, this.props.householdId))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(summary => this.setState({ summary
                                     }))
  }

  uncancelOrder = () => {
    this.props.request(ServerApi.command.uncancelHouseholdOrder(this.props.orderId, this.props.householdId))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(summary => this.setState({ summary
                                     }))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    if(!this.state.summary) return <div>Order not found.</div>

    let summary = this.state.summary
    let unusedProducts = this.state.products.filter(p => !summary.items.find(i => i.productId == p.id))

    return (
      <div>
        <div>
          <Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;
          <Link action={_ => this.props.navigate('/orders/' + this.props.orderId)}>{Util.formatDate(summary.orderCreatedDate)}</Link> &gt;
        </div>
        <h1>{summary.householdName} {summary.cancelled && ' (cancelled)'}</h1>
        <div>
          {!summary.orderComplete && (
            summary.cancelled
              ? <Link disabled={!!this.state.addingProduct} action={this.uncancelOrder}>Uncancel</Link>
              : <Link disabled={!!this.state.addingProduct} action={this.cancelOrder}>Cancel order</Link>
          )}
          {!summary.orderComplete && !summary.cancelled && 
            <Link disabled={!!this.state.addingProduct} action={_ => {}}>Record payment</Link>
          }
        </div>
        <div>
        {!summary.orderComplete && !summary.cancelled && !!unusedProducts.length &&
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
          {summary.items.map(i => this.state.editingProduct && this.state.editingProduct.id == i.productId 
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
              <span>x {i.quantity}</span>
              <Money amount={i.total} />
              {!summary.orderComplete && !summary.cancelled &&
                <span>
                  <Link disabled={!!this.state.addingProduct} action={() => this.startEdit(i)}>Edit</Link>
                  <Link disabled={!!this.state.addingProduct} action={() => this.delete(i)}>Delete</Link>
                </span>
              }
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={summary.total} />
          </div>
        </div>
      </div>
    )
  }
}