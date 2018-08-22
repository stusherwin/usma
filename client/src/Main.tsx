import * as React from 'react';

import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Router } from './Router'

import { OrdersPage } from './OrdersPage'
import { OrderPage } from './OrderPage'
import { PastOrderPage } from './PastOrderPage'
import { HouseholdOrderPage } from './HouseholdOrderPage'
import { HouseholdOrdersPage } from './HouseholdOrdersPage'
import { PastHouseholdOrderPage } from './PastHouseholdOrderPage'
import { FullOrderPage } from './FullOrderPage'
import { ProductsPage } from './ProductsPage'
import { HouseholdsPage } from './HouseholdsPage'
import { CollectiveOrder, HouseholdOrder, Product, Household } from './Types'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , initialUrl: string
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
                 , initialUrl: url
                 , url
                 , collectiveOrders: []
                 , householdOrders: []
                 , products: []
                 , households: []
                 , initialised: false
                 }
    
    this.router.route('/orders/{orderId}/households/{householdId}', c => {
      const householdOrder = this.state.householdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      return householdOrder && (householdOrder.isOrderPast
        ? <PastHouseholdOrderPage householdOrder={householdOrder} navigate={this.navigate} />
        : <HouseholdOrderPage householdOrder={householdOrder} products={this.state.products} reload={this.reload} request={this.request} navigate={this.navigate} />
      )
    })
    this.router.route('/orders/{orderId}/full', c => {
      const order = this.state.collectiveOrders.find(o => o.id == c.orderId)
      return order && <FullOrderPage order={order} request={this.request} navigate={this.navigate} />
    })
    this.router.route('/orders/{orderId}', c => {
      const order = this.state.collectiveOrders.find(o => o.id == c.orderId)
      const householdOrders = this.state.householdOrders.filter(o => o.orderId == c.orderId)
      return order && (order.isPast
        ? <PastOrderPage order={order} householdOrders={householdOrders} navigate={this.navigate} />
        : <OrderPage order={order} householdOrders={householdOrders} households={this.state.households} reload={this.reload} request={this.request} navigate={this.navigate} />
      )
    })
    this.router.route('/orders', _ => <OrdersPage orders={this.state.collectiveOrders} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/products', _ => <ProductsPage products={this.state.products} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/households/{householdId}', c => {
      const household = this.state.households.find(h => h.id == c.householdId)
      const householdOrders = this.state.householdOrders.filter(o => o.householdId == c.householdId)
      const currentCollectiveOrder = this.state.collectiveOrders.find(o => !o.isPast)
      return household && <HouseholdOrdersPage household={household} currentCollectiveOrder={currentCollectiveOrder} householdOrders={householdOrders} reload={this.reload} request={this.request} navigate={this.navigate} />
    })
    this.router.route('/households', _ => <HouseholdsPage households={this.state.households} reload={this.reload} request={this.request} navigate={this.navigate} />)
    this.router.route('/', _ => <div>Home page</div>)
  }

  componentDidMount() {
    window.onpopstate = e => this.setState({url: e.state || this.state.initialUrl})
    this.loadData().then(() => 
      this.setState({ initialised: true
                    })
    )
  }

  reload = () => {
    this.loadData()
  }

  loadData = () =>
    this.request(Promise.all([ ServerApi.query.collectiveOrders()
                             , ServerApi.query.householdOrders()
                             , ServerApi.query.products()
                             , ServerApi.query.households()
                             ]))
    .then(([collectiveOrders, householdOrders, products, households]) => {
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