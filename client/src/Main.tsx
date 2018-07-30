import * as React from 'react';

import { Order, Product, Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

export interface MainProps {}
export interface MainState { initialised: boolean
                           , loading: boolean
                           , error: ApiError | null
                           , orders: Order[]
                           , products: Product[]
                           , households: Household[]
                           , location: string | undefined
                           }

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    const urlParts = window.location.href.split('/').filter(l => l.length).slice(2)

    this.state = { initialised: false
                 , loading: false
                 , error: null
                 , orders: []
                 , products: []
                 , households: []
                 , location: urlParts[0]
                 }
  }

  componentDidMount() {
    this.request(Promise.all([ServerApi.getOrders(), ServerApi.getProducts(), ServerApi.getHouseholds()]))
      .then(results => {
        this.setState({ orders: results[0]
                      , products: results[1]
                      , households: results[2]
                      , initialised: true
                      })
      })
  }

  request = (p: Promise<any>) => {
    this.setState({loading: true})
    p.then(_ => this.setState({loading: false}))
     .catch(err => {
       let apiError = err as ApiError
       console.log(err)
       this.setState({ loading: false
                     , error: apiError
                     })
     })

    return p
  }

  body() {
    if(!this.state.initialised) return null

    switch(this.state.location) {
      case 'orders': return <OrdersPage orders={ this.state.orders } />
      case 'products': return <ProductsPage products={ this.state.products } />
      case 'households': return <HouseholdsPage households={ this.state.households } />
      default: return <div>Page not found</div>
    }
  }

  updatePath = (location: string) => (e: React.MouseEvent<HTMLAnchorElement>) => {
    e.preventDefault()
    window.history.pushState(location, location, location)
    this.setState({location})
  }

  render() {
    return (
      <div>
        <div>
          <a href="orders" onClick={this.updatePath('orders')}>Orders</a>
          <a href="products" onClick={this.updatePath('products')}>Products</a>
          <a href="households" onClick={this.updatePath('households')}>Households</a>
        </div>
        <div style={{visibility: this.state.loading? 'visible' : 'hidden'}}>Loading...</div>
        { this.body() }
      </div>
    )
  }
}

const OrdersPage = (props: { orders: Order[] }) => {
  const completeOrders = props.orders.filter(o => !o.complete)
  const pastOrders = props.orders.filter(o => o.complete)
  return (
    <div>
      <h1>Orders</h1>
      <a href="#" onClick={ e => e.preventDefault() }>New order</a>
      <h2>Open order</h2>
      <OrderList orders={completeOrders} btnText="Manage" none="No open order" />
      <h2>Past orders</h2>
      <OrderList orders={pastOrders} btnText="View" none="No past orders" />
    </div>
  )
}

const ProductsPage = (props: { products: Product[] }) => {
  return (
    <div>
      <h1>Products</h1>
    </div>
  )
}

const HouseholdsPage = (props: { households: Household[] }) => {
  return (
    <div>
      <h1>Households</h1>
    </div>
  )
}

const OrderList = (props: { orders: Order[], btnText: string, none: string }) => (
  !props.orders.length ? <div>{ props.none }</div> : (
    <div>
      { props.orders.map(o => (
        <div>
          <span>{ Util.formatDate(o.createdDate) }</span>
          <span>&pound;{ Util.formatMoney(o.total) }</span>
          <a href="#" onClick={ e => e.preventDefault() }>{ props.btnText }</a>
        </div>
      )) }
    </div>
  )
)