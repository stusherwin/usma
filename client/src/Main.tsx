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
  constructor(props: MainProps) {
    super(props)

    const url = '/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')
    Router.updateUrl(url)

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
  }

  componentDidMount() {
    // console.log('componentDidMount')

    window.onpopstate = e => {
      // console.log('onpopstate')
      const url = e.state || this.state.initialUrl
      Router.updateUrl(url)
      this.setState({url});
    }

    (window as any).history.onpushstate = (state: any, title: any, url: string) => {
      // console.log('onpushstate')
      Router.updateUrl(url)
      this.setState({ url })
    }
        
    this.reload().then(() => 
      this.setState({ initialised: true
                    })
    )
    
    const url = '/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')
    this.setState({url})
    Router.updateUrl(url)
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
    const router = new Router('')    
    router.route('/admin/orders/place', _ => {
      const currentOrder = this.state.collectiveOrder || undefined

      return currentOrder &&
        <AdminPlaceOrderPage currentOrder={currentOrder}
                             reload={this.reload}
                             request={this.request}
                             loading={this.state.loading}
                             error={this.state.error} />
    })
    
    router.route('/admin/orders/{orderId}/households/{householdId}', c => {
      const householdOrder = this.state.pastHouseholdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      
      return householdOrder && 
        <AdminPastHouseholdOrderPage referrer="order"
                                     householdOrder={householdOrder}
                                     loading={this.state.loading}
                                     error={this.state.error} />
    })

    router.route('/admin/orders/{orderId}', c => {
      const order = this.state.pastCollectiveOrders.find(o => o.id == c.orderId)
      const householdOrders = this.state.pastHouseholdOrders.filter(o => o.orderId == c.orderId)

      return order &&
        <AdminPastOrderPage order={order}
                            householdOrders={householdOrders}
                            loading={this.state.loading}
                            error={this.state.error} />
    })

    router.route('/admin/orders', c => {
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
    
    router.route('/admin/products', _ => <AdminProductsPage products={this.state.productCatalogue}
                                                                 reload={this.reload}
                                                                 request={this.request}
                                                                 loading={this.state.loading}
                                                                 error={this.state.error} />)
    
    router.route('/admin/households/{householdId}/orders/{orderId}', c => {
      const householdOrder = this.state.pastHouseholdOrders.find(o => o.orderId == c.orderId && o.householdId == c.householdId)
      
      return householdOrder && 
        <AdminPastHouseholdOrderPage referrer="household"
                                     householdOrder={householdOrder}
                                     loading={this.state.loading}
                                     error={this.state.error} />
    })

    router.route('/admin/households/{householdId}', c => {
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
    
    router.route('/admin/households', _ => <AdminHouseholdsPage households={this.state.households}
                                                                reload={this.reload}
                                                                request={this.request}
                                                                loading={this.state.loading}
                                                                error={this.state.error} />)
    
    router.route('/admin', _ => <AdminHomePage />)

    router.route('/households/{householdId}', (c, r) => {
      const household = this.state.households.find(h => h.id == c.householdId)
      const householdOrders = this.state.householdOrders.filter(o => o.householdId == c.householdId)
      const pastHouseholdOrders = this.state.pastHouseholdOrders.filter(o => o.householdId == c.householdId)
      const currentOrder = this.state.collectiveOrder
      const currentHouseholdOrder = currentOrder && (householdOrders.filter(o => o.orderId == currentOrder.id)[0] || null)
      const currentHouseholdOrders = currentOrder && householdOrders.filter(o => o.orderId == currentOrder.id) || []
      const householdPayments = this.state.householdPayments.filter(o => o.householdId == c.householdId)
      
      return household && 
        <HouseholdPage household={household}
                       currentOrder={currentOrder}
                       currentHouseholdOrder={currentHouseholdOrder}
                       currentHouseholdOrders={currentHouseholdOrders}
                       pastHouseholdOrders={pastHouseholdOrders}
                       payments={householdPayments}
                       products={this.state.productCatalogue}
                       households={this.state.households}
                       reload={this.reload}
                       request={this.request}
                       loading={this.state.loading}
                       error={this.state.error}
                       router={r} />
    })

    router.route('/', _ => <WelcomePage households={this.state.households}
                                             request={this.request}
                                             reload={this.reload}
                                             loading={this.state.loading }
                                             error={this.state.error} />)

    return this.state.initialised
      ? router.resolve()
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
}