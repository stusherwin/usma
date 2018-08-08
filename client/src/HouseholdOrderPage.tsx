import * as React from 'react';

import { HouseholdOrderSummary, Product, HouseholdOrderSummary_Item } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface HouseholdOrderPageProps { orderId: number
                                         , householdId: number
                                         , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                         , navigate: (location: string) => void
                                         }

export interface HouseholdOrderPageState { details: HouseholdOrderSummary | null
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

    this.state = { details: null
                 , products: []
                 , initialised: false
                 , addingProduct: null
                 , addingProductQuantity: 1
                 , editingProduct: null
                 , editingProductQuantity: 1
                 }
  }

  componentDidMount() {
    console.log(this.props)
    this.props.request(Promise.all([ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId), ServerApi.query.products()]))
      .then(results => { console.log(results); this.setState({ details: results[0]
                                     , products: results[1]
                                     , initialised: true
                                     })})
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
      .then(details => this.setState({ details
                                     , addingProduct: null
                                     , addingProductQuantity: 1
                                     }))
  }

  delete = (item: HouseholdOrderSummary_Item) => {
    this.props.request(ServerApi.command.removeHouseholdOrderItem(this.props.orderId, this.props.householdId, item.productId))
      .then(() => this.props.request(ServerApi.query.householdOrderSummary(this.props.orderId, this.props.householdId)))
      .then(details => this.setState({ details
                                     }))
  }

  startEdit = (item: HouseholdOrderSummary_Item) => {
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
      .then(details => this.setState({ details
                                     , editingProduct: null
                                     , editingProductQuantity: 1
                                     }))
  }

  cancelEdit = () =>
    this.setState({ editingProduct: null
                  , editingProductQuantity: 1
                  })

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    if(!this.state.details) return <div>Order not found.</div>

    let details = this.state.details
    let unusedProducts = this.state.products.filter(p => !details.items.find(i => i.productId == p.id))

    return (
      <div>
        <div>
          <Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;
          <Link action={_ => this.props.navigate('/orders/' + this.props.orderId)}>{Util.formatDate(details.orderCreatedDate)}</Link> &gt;
        </div>
        <h1>{details.householdName}</h1>
        <div>
          {details.status != 'cancelled' ? <Link disabled={!!this.state.addingProduct} action={_ => {}}>Cancel order</Link> : null}
          <Link disabled={!!this.state.addingProduct} action={_ => {}}>Record payment</Link>
        </div>
        {!!unusedProducts.length && <div><Link action={() => this.startAdd(unusedProducts[0])}>Add</Link></div>}
        <div>
          {this.state.addingProduct && <div>
            <span>
              <select value={this.state.addingProduct.id} onChange={this.addingProductChanged}>
                {unusedProducts.map(p => <option value={p.id}>{p.name}</option>)}
              </select>
            </span>
            <span>
              <select value={this.state.addingProductQuantity} onChange={this.addingQuantityChanged}>
                {[1,2,3,4,5,6,7,8,9,10].map(q => <option value={q}>x {q}</option>)}
              </select>
            </span>
            <Money amount={this.state.addingProduct.price * this.state.addingProductQuantity} />
            <Link action={this.confirmAdd}>Add</Link>
            <Link action={this.cancelAdd}>Cancel</Link>
          </div>}
          {details.items.map(i => this.state.editingProduct && this.state.editingProduct.id == i.productId 
          ? (
            <div>
              <span>{i.productName}</span>
              <span>
                <select value={this.state.editingProductQuantity} onChange={this.editingQuantityChanged}>
                  {[1,2,3,4,5,6,7,8,9,10].map(q => <option value={q}>x {q}</option>)}
                </select>
              </span>
              <Money amount={this.state.editingProduct.price * this.state.editingProductQuantity} />
              <Link action={this.confirmEdit}>Save</Link>
              <Link action={this.cancelEdit}>Cancel</Link>
            </div>
          )
          : (
            <div>
              <span>{i.productName}</span>
              <span>x {i.quantity}</span>
              <Money amount={i.total} />
              <Link disabled={!!this.state.addingProduct} action={() => this.startEdit(i)}>Edit</Link>
              <Link disabled={!!this.state.addingProduct} action={() => this.delete(i)}>Delete</Link>
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={details.total} />
          </div>
        </div>
      </div>
    )
  }
}