import * as React from 'react';

import { Order, Product, Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , url: string
                           }

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    const url = window.location.href.split('/').filter(l => l.length).slice(2).join('/')

    this.state = { loading: false
                 , error: null
                 , url
                 }
  }

  request = <T extends {}>(p: Promise<T>) => {
    this.setState({ loading: true })
    p.then(_ => this.setState({ loading: false }))
     .catch(err => {
       let apiError = err as ApiError
       console.log(err)
       this.setState({ loading: false
                     , error: apiError
                     })
     })

    return p
  }

  navigate = (url: string) => {
    window.history.pushState(url, url, '/' + url)
    this.setState({ url })
  }

  body() {
    let urlParts = this.state.url.split('/')
    let location = urlParts[0]

    switch(location) {
      case 'orders': return <OrdersPage request={this.request} urlParts={urlParts.slice(1)} navigate={this.navigate} />
      case 'products': return <ProductsPage request={this.request} navigate={this.navigate} />
      case 'households': return <HouseholdsPage request={this.request} navigate={this.navigate} />
      default: return <div>Page not found</div>
    }
  }

  render() {
    return (
      <div>
        <div>
          <a href="orders" onClick={e => {e.preventDefault(); this.navigate('orders')}}>Orders</a>
          <a href="products" onClick={e => {e.preventDefault(); this.navigate('products')}}>Products</a>
          <a href="households" onClick={e => {e.preventDefault(); this.navigate('households')}}>Households</a>
        </div>
        <div style={{visibility: this.state.loading? 'visible' : 'hidden'}}>Loading...</div>
        { this.body() }
      </div>
    )
  }
}

export interface OrdersPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , navigate: (location: string) => void
                                 , urlParts: string[]
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, {}> {
  navigate = (url: string) => {
    this.props.navigate('orders/' + url)
  }

  render() {
    let orderId = parseInt(this.props.urlParts[0]) || null
    let urlRemainder = this.props.urlParts.slice(1)

    if(orderId) return <OrderPage id={orderId} request={this.props.request} navigate={this.navigate} />

    return (
      <div>
        <OrderList request={this.props.request} navigate={this.navigate} />
      </div>
    )
  }
}

export interface OrderListProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export class OrderList extends React.Component<OrderListProps, { orders: Order[], initialised: boolean }> {
  constructor(props: OrderListProps) {
    super(props)

    this.state = { orders: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getOrders())
      .then(orders => {
        this.setState({ orders
                      , initialised: true
                      })
      })
  }

  newOrder = () => {
    this.props.request(ServerApi.newOrder()).then(id => this.props.navigate('' + id))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    const currentOrder = this.state.orders.filter(o => !o.complete)[0]
    const pastOrders = this.state.orders.filter(o => o.complete)
    return (
      <div>
        <h1>Orders</h1>
        {!currentOrder? <a href="#" onClick={e => {e.preventDefault(); this.newOrder()}}>New order</a> : (
          <div>
            <h2>Current order</h2>
            <div>
              <div>
                <span>{ Util.formatDate(currentOrder.createdDate) }</span>
                <span>&pound;{ Util.formatMoney(currentOrder.total) }</span>
                <a href="#" onClick={e => {e.preventDefault(); this.props.navigate('' + currentOrder.id)}}>Manage</a>
              </div>
            </div>
          </div>
        )}
        <h2>Past orders</h2>
        {!pastOrders.length ? <div>No past orders</div> : (
          <div>
            { pastOrders.map(o => (
              <div key={o.id}>
                <span>{ Util.formatDate(o.createdDate) }</span>
                <span>&pound;{ Util.formatMoney(o.total) }</span>
                <a href="#" onClick={e => {e.preventDefault(); this.props.navigate('' + o.id)}}>View</a>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}

export interface OrderPageProps { id: number
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export interface OrderPageState { order: Order | null
                                , initialised: boolean
                                }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { order: null
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getOrder(this.props.id))
      .then(order => this.setState({ order
                                   , initialised: true
                                   }))
      .catch(_ => this.setState({ initialised: true }))
  }

  delete = (e: React.SyntheticEvent) => {
    e.preventDefault()
    this.props.request(ServerApi.deleteOrder(this.props.id)).then(_ => this.props.navigate(''))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    if(!this.state.order) return <div>Order not found.</div>

    return (
      <div>
        <h1>Order: {Util.formatDate(this.state.order.createdDate)}</h1>
        <a href="#" onClick={this.delete}>Delete</a>
      </div>
    )
  }
}

export interface ProductsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , navigate: (location: string) => void
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, { products: Product[], initialised: boolean }> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { products: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getProducts())
      .then(products => {
        this.setState({ products
                      , initialised: true
                      })
      })
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>

    return (
      <div>
        <h1>Products</h1>
      </div>
    )
  }
}

export interface HouseholdsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , navigate: (location: string) => void
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, { households: Household[], initialised: boolean }> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { households: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getHouseholds())
      .then(households => {
        this.setState({ households
                      , initialised: true
                      })
      })
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>

    return (
      <div>
        <h1>Households</h1>
      </div>
    )
  }
}