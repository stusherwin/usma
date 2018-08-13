import * as React from 'react';

import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Router } from './Router'

import { OrdersPage } from './OrdersPage'
import { OrderPage } from './OrderPage'
import { HouseholdOrderPage } from './HouseholdOrderPage'
import { FullOrderPage } from './FullOrderPage'
import { ProductsPage } from './ProductsPage'
import { HouseholdsPage } from './HouseholdsPage'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , url: string
                           }

export class Main extends React.Component<MainProps, MainState> {
  router = new Router()

  constructor(props: MainProps) {
    super(props)

    const url = '/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')

    this.state = { loading: false
                 , error: null
                 , url
                 }
    
    this.router.route('/orders/{orderId}/households/{householdId}', c => <HouseholdOrderPage orderId={c.orderId} householdId={c.householdId} request={this.request} navigate={this.navigate} />)
    this.router.route('/orders/{orderId}/full', c => <FullOrderPage orderId={c.orderId} request={this.request} navigate={this.navigate} />)
    this.router.route('/orders/{orderId}', c => <OrderPage id={c.orderId} request={this.request} navigate={this.navigate} />)
    this.router.route('/orders', _ => <OrdersPage request={this.request} navigate={this.navigate} />)
    this.router.route('/products', _ => <ProductsPage request={this.request} navigate={this.navigate} />)
    this.router.route('/households', _ => <HouseholdsPage request={this.request} navigate={this.navigate} />)
  }

  componentDidMount() {
    window.onpopstate = e => this.setState({url: e.state})
  }

  request = <T extends {}>(p: Promise<T>) => {
    this.setState({ loading: true })
    p.then(_ => this.setState({ loading: false }))
     .catch(err => {
       const apiError = err as ApiError
       console.log(err)
       this.setState({ loading: false
                     , error: apiError
                     })
     })

    return p
  }

  navigate = (url: string) => {
    window.history.pushState(url, url, url)
    this.setState({ url })
  }

  render() {
    return (
      <div>
        <div>
          <Link action={_ => this.navigate('/orders')}>Orders</Link>
          <Link action={_ => this.navigate('/products')}>Products</Link>
          <Link action={_ => this.navigate('/households')}>Households</Link>
        </div>
        <div style={{visibility: this.state.loading? 'visible' : 'hidden'}}>Loading...</div>
        { this.body() }
      </div>
    )
  }

  body() {
    return this.router.resolve(this.state.url)
  }
}