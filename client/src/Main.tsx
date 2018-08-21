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
import { CollectiveOrder, HouseholdOrder, Product, Household } from './Types'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , url: string
                           , collectiveOrders: CollectiveOrder[]
                           , householdOrders: HouseholdOrder[]
                           , products: Product[]
                           , households: Household[]
                           , initialised: boolean
                           }

export class Main extends React.Component<MainProps, MainState> {
  router = new Router()

  constructor(props: MainProps) {
    super(props)

    const url = '/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')

    this.state = { loading: false
                 , error: null
                 , url
                 , collectiveOrders: []
                 , householdOrders: []
                 , products: []
                 , households: []
                 , initialised: false
                 }
    
    this.router.route('/orders/{orderId}/households/{householdId}', c => {
      const order = this.state.householdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      return order && <HouseholdOrderPage order={order} products={this.state.products} reload={this.reload} request={this.request} navigate={this.navigate} />
    })
    this.router.route('/orders/{orderId}/full', c => {
      const order = this.state.collectiveOrders.find(o => o.id == c.orderId)
      return order && <FullOrderPage order={order} request={this.request} navigate={this.navigate} />
    })
    this.router.route('/orders/{orderId}', c => {
      const order = this.state.collectiveOrders.find(o => o.id == c.orderId)
      return order && <OrderPage order={order} households={this.state.households} reload={this.reload} request={this.request} navigate={this.navigate} />
    })
    this.router.route('/orders', _ => <OrdersPage orders={this.state.collectiveOrders} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/products', _ => <ProductsPage products={this.state.products} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/households', _ => <HouseholdsPage households={this.state.households} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/', _ => <div>Home page</div>)
  }

  componentDidMount() {
    window.onpopstate = e => this.setState({url: e.state})
    this.loadData().then(() => 
      this.setState({ initialised: true
                    })
    )
  }

  reload = () => {
    this.loadData()
  }

  loadData = () => this.request(Promise.all([ ServerApi.query.collectiveOrders()
                           , ServerApi.query.householdOrders()
                           , ServerApi.query.products()
                           , ServerApi.query.households()
                           ]))
    .then(([collectiveOrders, householdOrders, products, households]) => {
      for(let o of collectiveOrders) {
        o.householdOrders = o.householdIds.map(id => householdOrders.find(h => h.orderId == o.id && h.householdId == id))
                                          .filter(h => !!h)
                                          .map(h => h as HouseholdOrder)
      }
      this.setState({ collectiveOrders
                    , householdOrders
                    , products
                    , households
                    })
    })

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
        { this.state.initialised && (
          <div>
            <Link action={_ => this.navigate('/orders')}>Orders</Link>
            <Link action={_ => this.navigate('/products')}>Products</Link>
            <Link action={_ => this.navigate('/households')}>Households</Link>
          </div>
        ) }
        <div style={{visibility: this.state.loading? 'visible' : 'hidden'}}>Loading...</div>
        { this.state.initialised && this.body() }
      </div>
    )
  }

  body() {
    return this.router.resolve(this.state.url)
  }
}