import * as React from 'react';

import { ServerApi, ApiError } from './ServerApi'
import { Util } from './common/Util'
import { RouterLink } from './common/RouterLink'
import { Icon } from './common/Icon'
import { Router } from './common/Router'

import { OrdersPage as AdminOrdersPage } from './admin/order/OrdersPage'
import { PastOrderPage as AdminPastOrderPage } from './admin/order/PastOrderPage'
import { HouseholdPage as AdminHouseholdPage } from './admin/household/HouseholdPage'
import { PastHouseholdOrderPage as AdminPastHouseholdOrderPage } from './admin/household/PastHouseholdOrderPage'
import { PlaceOrderPage as AdminPlaceOrderPage } from './admin/order/PlaceOrderPage'
import { ProductsPage as AdminProductsPage } from './admin/product/ProductsPage'
import { HouseholdsPage as AdminHouseholdsPage } from './admin/household/HouseholdsPage'
import { HomePage as AdminHomePage } from './admin/HomePage'
import { WelcomePage } from './household/WelcomePage'
import { HouseholdPage } from './household/HouseholdPage'
import { CollectiveOrder, PastCollectiveOrder, HouseholdOrder, PastHouseholdOrder, Product, Household, HouseholdPayment, ProductCatalogueEntry } from './Types'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , initialUrl: string
                           , url: string
                           , collectiveOrder: CollectiveOrder | null
                           , pastCollectiveOrders: PastCollectiveOrder[]
                           , householdOrders: HouseholdOrder[]
                           , pastHouseholdOrders: PastHouseholdOrder[]
                           , productCatalogue: ProductCatalogueEntry[]
                           , households: Household[]
                           , householdPayments: HouseholdPayment[]
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
                 , collectiveOrder: null
                 , pastCollectiveOrders: []
                 , householdOrders: []
                 , pastHouseholdOrders: []
                 , productCatalogue: []
                 , households: []
                 , householdPayments: []
                 , initialised: false
                 }

    this.router.route('/admin/orders/place', _ => {
      const currentOrder = this.state.collectiveOrder || undefined

      return currentOrder &&
        <AdminPlaceOrderPage currentOrder={currentOrder}
                             reload={this.reload}
                             request={this.request}
                             loading={this.state.loading}
                             error={this.state.error} />
    })
    
    this.router.route('/admin/orders/{orderId}/households/{householdId}', c => {
      const householdOrder = this.state.pastHouseholdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      
      return householdOrder && 
        <AdminPastHouseholdOrderPage referrer="order"
                                     householdOrder={householdOrder}
                                     loading={this.state.loading}
                                     error={this.state.error} />
    })

    this.router.route('/admin/orders/{orderId}', c => {
      const order = this.state.pastCollectiveOrders.find(o => o.id == c.orderId)
      const householdOrders = this.state.pastHouseholdOrders.filter(o => o.orderId == c.orderId)

      return order &&
        <AdminPastOrderPage order={order}
                            householdOrders={householdOrders}
                            loading={this.state.loading}
                            error={this.state.error} />
    })

    this.router.route('/admin/orders', c => {
      const currentOrder = this.state.collectiveOrder
      const currentHouseholdOrders = this.state.householdOrders.filter(o => currentOrder && o.orderId == currentOrder.id)
      const pastOrders = this.state.pastCollectiveOrders

      return <AdminOrdersPage currentOrder={currentOrder}
                              currentHouseholdOrders={currentHouseholdOrders}
                              households={this.state.households}
                              pastOrders={pastOrders}
                              reload={this.reload}
                              request={this.request}
                              loading={this.state.loading}
                              error={this.state.error} />
    })
    
    this.router.route('/admin/products', _ => <AdminProductsPage products={this.state.productCatalogue}
                                                                 reload={this.reload}
                                                                 request={this.request}
                                                                 loading={this.state.loading}
                                                                 error={this.state.error} />)
    
    this.router.route('/admin/households/{householdId}/orders/{orderId}', c => {
      const householdOrder = this.state.pastHouseholdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      
      return householdOrder && 
        <AdminPastHouseholdOrderPage referrer="household"
                                     householdOrder={householdOrder}
                                     loading={this.state.loading}
                                     error={this.state.error} />
    })

    this.router.route('/admin/households/{householdId}', c => {
      const household = this.state.households.find(h => h.id == c.householdId)
      const householdOrders = this.state.householdOrders.filter(o => o.householdId == c.householdId)
      const pastHouseholdOrders = this.state.pastHouseholdOrders.filter(o => o.householdId == c.householdId)
      const currentCollectiveOrder = this.state.collectiveOrder
      const currentHouseholdOrder = currentCollectiveOrder && (householdOrders.filter(o => o.orderId == currentCollectiveOrder.id)[0] || null)
      const householdPayments = this.state.householdPayments.filter(o => o.householdId == c.householdId)
      
      return household && 
        <AdminHouseholdPage household={household}
                            currentCollectiveOrder={currentCollectiveOrder}
                            currentHouseholdOrder={currentHouseholdOrder}
                            pastHouseholdOrders={pastHouseholdOrders}
                            payments={householdPayments}
                            products={this.state.productCatalogue}
                            reload={this.reload}
                            request={this.request}
                            loading={this.state.loading}
                            error={this.state.error} />
    })
    
    this.router.route('/admin/households', _ => <AdminHouseholdsPage households={this.state.households}
                                                                     reload={this.reload}
                                                                     request={this.request}
                                                                     loading={this.state.loading}
                                                                     error={this.state.error} />)
    
    this.router.route('/admin', _ => <AdminHomePage />)

    this.router.route('/households/{householdId}', c => {
      const household = this.state.households.find(h => h.id == c.householdId)
      const householdOrders = this.state.householdOrders.filter(o => o.householdId == c.householdId)
      const pastHouseholdOrders = this.state.pastHouseholdOrders.filter(o => o.householdId == c.householdId)
      const currentCollectiveOrder = this.state.collectiveOrder
      const currentHouseholdOrder = currentCollectiveOrder && (householdOrders.filter(o => o.orderId == currentCollectiveOrder.id)[0] || null)
      const householdPayments = this.state.householdPayments.filter(o => o.householdId == c.householdId)
      
      return household && 
        <HouseholdPage household={household}
                       currentCollectiveOrder={currentCollectiveOrder}
                       currentHouseholdOrder={currentHouseholdOrder}
                       pastHouseholdOrders={pastHouseholdOrders}
                       payments={householdPayments}
                       products={this.state.productCatalogue}
                       reload={this.reload}
                       request={this.request}
                       loading={this.state.loading}
                       error={this.state.error} />
    })

    this.router.route('/', _ => <WelcomePage households={this.state.households}
                                             request={this.request}
                                             reload={this.reload}
                                             loading={this.state.loading }
                                             error={this.state.error} />)
  }

  componentDidMount() {
    window.onpopstate = e => this.setState({url: e.state || this.state.initialUrl});
    (window as any).history.onpushstate = (state: any, title: any, url: string) => this.setState({ url })
        
    this.reload().then(() => 
      this.setState({ initialised: true
                    })
    )
  }

  reload = () =>
    this.request(Promise.all([ ServerApi.query.collectiveOrder()
                             , ServerApi.query.pastCollectiveOrders()
                             , ServerApi.query.householdOrders()
                             , ServerApi.query.pastHouseholdOrders()
                             , ServerApi.query.households()
                             , ServerApi.query.householdPayments()
                             , ServerApi.query.productCatalogue()
                             ]))
    .then(([collectiveOrder, pastCollectiveOrders, householdOrders, pastHouseholdOrders, households, householdPayments, productCatalogue]) => {
      this.setState({ collectiveOrder
                    , pastCollectiveOrders
                    , householdOrders
                    , pastHouseholdOrders
                    , households
                    , householdPayments
                    , productCatalogue
                    })
    })

  request = <T extends {}>(p: Promise<T>) => {
    this.setState({ loading: true })
    p.then(_ => this.setState({ loading: false }))
     .then()
     .catch(err => {
       const apiError = err as ApiError
       console.log(err)
       this.setState({ loading: false
                     , error: apiError
                     })
     })

    return p
  }

  render() {
    return this.state.initialised
      ? this.body()
      : (
        <div>
          {!!this.state.loading && 
            <div className="h-screen flex items-center justify-center text-grey-light">
              <Icon type="loading" className="w-16 h-16 -mt-4 rotating fill-current" />
            </div>
          }
          {!!this.state.error && (
            <div>{this.state.error.error}: {this.state.error.message}</div>
          )}
        </div>
    )
  }

  body() {
    return this.router.resolve(this.state.url)
  }
}