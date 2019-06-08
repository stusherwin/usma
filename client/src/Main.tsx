import * as React from 'react';

import { ServerApi, ApiError } from './ServerApi'
import { Router } from './common/Router'

import { AdminOrdersPage } from './pages/AdminOrdersPage'
import { AdminHouseholdPage } from './pages/AdminHouseholdPage'
import { AdminPlaceOrderPage } from './pages/AdminPlaceOrderPage'
import { AdminProductsPage } from './pages/AdminProductsPage'
import { AdminHouseholdsPage } from './pages/AdminHouseholdsPage'
import { AdminHomePage } from './pages/AdminHomePage'
import { HouseholdWelcomePage } from './pages/HouseholdWelcomePage'
import { HouseholdPage } from './pages/HouseholdPage'
import { CollectiveOrder, PastCollectiveOrder, HouseholdOrder, PastHouseholdOrder, Household, HouseholdPayment, ProductCatalogueEntry } from './Types'
import { Loading } from './common/Loading'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , initialUrl: string
                           , url: string
                           , groupKey: string | null
                           , groupValid: boolean
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

    const urlInfo = this.parseUrl(this.currentUrl())
    Router.updatePath(urlInfo.path)

    this.state = { loading: false
                 , error: null
                 , initialUrl: urlInfo.url
                 , url: urlInfo.url
                 , groupKey: urlInfo.groupKey
                 , groupValid: false
                 , collectiveOrder: null
                 , pastCollectiveOrders: []
                 , householdOrders: []
                 , pastHouseholdOrders: []
                 , productCatalogue: []
                 , households: []
                 , householdPayments: []
                 , initialised: !urlInfo.groupKey
                 }
  }

  currentUrl = () => {
    return window.location.href.split('/').filter(l => l.length).slice(2).join('/')
  }

  parseUrl = (url: string) => {
    // console.log('url: ' + url)
    const urlParts = url.split('/').filter(l => l.length)
    // console.log('urlParts: ' + urlParts)
    const groupKey = urlParts.length > 1 && urlParts[0] == 'g' ? urlParts[1] : null;
    // console.log('groupKey: ' + groupKey)
    return { url, path: '/' + urlParts.slice(2).join('/'), groupKey }
  }

  componentDidMount() {
    window.onpopstate = e => {
      const url = e.state || this.state.initialUrl
      // console.log('onpopstate')
      // console.log(e.state)
      const urlInfo = this.parseUrl(url)
      Router.updatePath(urlInfo.path)
      this.setState({url: urlInfo.url, groupKey: urlInfo.groupKey});
    }

    (window as any).history.onpushstate = (state: any, title: any, url: string) => {
      // console.log('onpushstate')
      const urlInfo = this.parseUrl(url)
      Router.updatePath(urlInfo.path)
      this.setState({url: urlInfo.url, groupKey: urlInfo.groupKey});
    }

    if(this.state.groupKey) {
      this.request(ServerApi.verifyGroup(this.state.groupKey))
      .then(groupValid => {
        this.setState({ groupValid, initialised: !groupValid })
        if(groupValid) {
          this.reload().then(() => 
            this.setState({ initialised: true
                          })
          )
        }
      })
    }
    
    const urlInfo = this.parseUrl(this.currentUrl()) //'/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')
    this.setState({url: urlInfo.url, groupKey: urlInfo.groupKey})
    Router.updatePath(urlInfo.path)
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
      const currentHouseholdOrders = this.state.householdOrders.filter(o => currentOrder && o.orderId == currentOrder.id)

      return currentOrder &&
        <AdminPlaceOrderPage currentOrder={currentOrder}
                             currentHouseholdOrders={currentHouseholdOrders}
                             reload={this.reload}
                             request={this.request}
                             loading={this.state.loading}
                             error={this.state.error} />
    })
    
    router.route('/admin/orders', c => {
      const currentOrder = this.state.collectiveOrder
      const currentHouseholdOrders = this.state.householdOrders.filter(o => currentOrder && o.orderId == currentOrder.id)

      return <AdminOrdersPage currentOrder={currentOrder}
                              currentHouseholdOrders={currentHouseholdOrders}
                              households={this.state.households}
                              pastOrders={this.state.pastCollectiveOrders}
                              pastHouseholdOrders={this.state.pastHouseholdOrders}
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
    
    router.route('/admin/households/{householdId}', c => {
      const household = this.state.households.find(h => h.id == c.householdId)
      const householdOrders = this.state.householdOrders.filter(o => o.householdId == c.householdId)
      const pastHouseholdOrders = this.state.pastHouseholdOrders.filter(o => o.householdId == c.householdId)
      const currentOrder = this.state.collectiveOrder
      const currentHouseholdOrder = currentOrder && (householdOrders.filter(o => o.orderId == currentOrder.id)[0] || null)
      const currentHouseholdOrders = currentOrder && householdOrders.filter(o => o.orderId == currentOrder.id) || []
      const householdPayments = this.state.householdPayments.filter(o => o.householdId == c.householdId)
      
      return household && 
        <AdminHouseholdPage household={household}
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
      const householdOrders = this.state.householdOrders
      const pastHouseholdOrders = this.state.pastHouseholdOrders.filter(o => o.householdId == c.householdId)
      const currentOrder = this.state.collectiveOrder
      const currentHouseholdOrder = currentOrder && (householdOrders.filter(o => o.householdId == c.householdId && o.orderId == currentOrder.id)[0] || null)
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

    router.route('/households', _ => <HouseholdWelcomePage households={this.state.households}
                                                           request={this.request}
                                                           reload={this.reload}
                                                           loading={this.state.loading }
                                                           error={this.state.error} />)
    
    router.route('/$', _ => <HouseholdWelcomePage households={this.state.households}
                                                  request={this.request}
                                                  reload={this.reload}
                                                  loading={this.state.loading }
                                                  error={this.state.error} />)
    
    return (
      <div>
        {this.state.initialised
        ? (
            this.state.groupKey && this.state.groupValid 
            ? router.resolve() 
            : <div className="p-2">Group not found</div>
          )
        : <div>
            {!!this.state.error && (
              <div>{this.state.error.error}: {this.state.error.message}</div>
            )}
          </div>
        }
        <Loading loading={this.state.loading}></Loading>
      </div>
    )
  }
}