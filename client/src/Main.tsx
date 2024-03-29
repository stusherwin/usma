import * as React from 'react';

import { CollectiveOrder, Household, ProductCatalogueEntry, GroupSettings } from './util/Types'
import { ServerApi, ApiError } from './util/ServerApi'
import { Router } from './util/Router'
import { Loading } from './util/Loading'
import { Icon, Icons } from './util/Icon'

import { AdminOrdersPage } from 'admin/orders/AdminOrdersPage'
import { AdminProductsPage } from 'admin/products/AdminProductsPage'
import { AdminHouseholdPage } from 'admin/households/AdminHouseholdPage'
import { AdminHouseholdsPage } from 'admin/households/AdminHouseholdsPage'

import { HouseholdsPage } from './household/HouseholdsPage'
import { HouseholdPage } from './household/HouseholdPage'

export interface MainProps { }
export interface MainState {
  loading: boolean
  error: ApiError | null
  initialUrl: string
  url: string
  groupKey: string | null
  groupValid: boolean
  initialised: boolean
  collectiveOrders: CollectiveOrder[]
  productCatalogue: ProductCatalogueEntry[]
  categories: string[]
  brands: string[]
  households: Household[]
  groupSettings: GroupSettings
  productImage: string | null
  productTitle: string | null
  productUrl: string | null
  images: string[]
}

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    const urlInfo = this.parseUrl(this.currentUrl())
    Router.updatePath(urlInfo.path)

    this.state = {
      loading: false,
      error: null,
      initialUrl: urlInfo.url,
      url: urlInfo.url,
      groupKey: urlInfo.groupKey,
      groupValid: false,
      collectiveOrders: [],
      productCatalogue: [],
      categories: [],
      brands: [],
      households: [],
      initialised: !urlInfo.groupKey,
      groupSettings: { enablePayments: false },
      productImage: null,
      productTitle: null,
      productUrl: null,
      images: []
    }
  }

  currentUrl = () => {
    return window.location.href.split('/').filter(l => l.length).slice(2).join('/')
  }

  parseUrl = (url: string) => {
    // console.log('url: ' + url)
    const urlParts = url.split('/').filter(l => l.length)
    // console.log('urlParts: ' + urlParts)
    const groupKey = urlParts.length > 1 && urlParts[0] == 'g' || urlParts[0] == 'api' ? urlParts[1] : null;
    // console.log('groupKey: ' + groupKey)
    return { url, path: '/' + urlParts.slice(2).join('/'), groupKey }
  }

  componentDidMount() {
    window.onpopstate = e => {
      const url = e.state || this.state.initialUrl
      // console.log('onpopstate')
      // console.log(url)
      const urlInfo = this.parseUrl(url)
      Router.updatePath(urlInfo.path)
      this.setState({ url: urlInfo.url, groupKey: urlInfo.groupKey });
    }

    (window as any).history.onpushstate = (state: any, title: any, url: string) => {
      // console.log('onpushstate')
      // console.log(url)
      const urlInfo = this.parseUrl(url)
      Router.updatePath(urlInfo.path)
      this.setState({ url: urlInfo.url, groupKey: urlInfo.groupKey });
    }

    if (this.state.groupKey) {
      this.request(ServerApi.verifyGroup(this.state.groupKey))
        .then(groupValid => {
          this.setState({ groupValid, initialised: !groupValid })
          if (groupValid) {
            this.reload(true).then(() =>
              this.setState({
                initialised: true
              })
            )
          }
        })
    }

    const urlInfo = this.parseUrl(this.currentUrl()) //'/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')
    this.setState({ url: urlInfo.url, groupKey: urlInfo.groupKey })
    Router.updatePath(urlInfo.path)
  }

  reload = (reloadProoductCatalogue: boolean = false) => {
    return reloadProoductCatalogue === true
      ? this.request(Promise.all([ServerApi.query.getData(), ServerApi.query.getCatalogueData()]))
        .then(([data, catalogueData]) => this.setState({
          collectiveOrders: data.collectiveOrders,
          households: data.households,
          productCatalogue: catalogueData.productCatalogue,
          categories: catalogueData.categories,
          brands: catalogueData.brands,
          groupSettings: data.groupSettings
        }))
      : this.request(ServerApi.query.getData())
        .then(data => this.setState({
          collectiveOrders: data.collectiveOrders,
          households: data.households,
          groupSettings: data.groupSettings
        }))
  }

  request = <T extends {}>(p: Promise<T>) => {
    this.setState({ loading: true })
    console.log('setting loading to true');

    p.then(_ => { console.log('setting loading to false'); this.setState({ loading: false }) })
      .catch(err => {
        const apiError = err as ApiError
        console.log(err)
        this.setState({
          loading: false
          , error: apiError
        })
      })

    return p
  }

  showProductImage = (productCode: string) => {
    this.setState({ productImage: productCode, productTitle: null, productUrl: null });
    ServerApi.query.getProductInfo(productCode)
      .then(info => this.setState({ productTitle: info.title, productUrl: info.url }));
  }

  render() {
    const router = new Router('')

    router.route('/admin/orders', c =>
      <AdminOrdersPage
        collectiveOrder={this.state.collectiveOrders[0]}
        households={this.state.households}
        pastOrders={this.state.collectiveOrders.slice(1)}
        products={this.state.productCatalogue}
        categories={this.state.categories}
        brands={this.state.brands}
        groupSettings={this.state.groupSettings}
        reload={this.reload}
        request={this.request}
        showProductImage={this.showProductImage} />
    )

    router.route('/admin/products', _ =>
      <AdminProductsPage
        products={this.state.productCatalogue}
        categories={this.state.categories}
        brands={this.state.brands}
        reload={this.reload}
        request={this.request}
        showProductImage={this.showProductImage} />
    )

    router.route('/admin/households/{householdId}', c => {
      const household = this.state.households.find(h => h.id == c.householdId)

      return household &&
        <AdminHouseholdPage
          household={household}
          collectiveOrder={this.state.collectiveOrders[0]}
          products={this.state.productCatalogue}
          categories={this.state.categories}
          brands={this.state.brands}
          households={this.state.households}
          groupSettings={this.state.groupSettings}
          reload={this.reload}
          request={this.request}
          showProductImage={this.showProductImage} />
    })

    router.route('/admin/households', _ =>
      <AdminHouseholdsPage
        households={this.state.households}
        groupSettings={this.state.groupSettings}
        reload={this.reload}
        request={this.request} />)

    router.route('/admin', _ =>
      <AdminOrdersPage
        collectiveOrder={this.state.collectiveOrders[0]}
        households={this.state.households}
        pastOrders={this.state.collectiveOrders.slice(1)}
        products={this.state.productCatalogue}
        categories={this.state.categories}
        brands={this.state.brands}
        groupSettings={this.state.groupSettings}
        reload={this.reload}
        request={this.request}
        showProductImage={this.showProductImage} />
    )

    router.route('/households/{householdId}', (c, r) => {
      const household = this.state.households.find(h => h.id == c.householdId)

      return household &&
        <HouseholdPage
          household={household}
          collectiveOrder={this.state.collectiveOrders[0]}
          products={this.state.productCatalogue}
          categories={this.state.categories}
          brands={this.state.brands}
          households={this.state.households}
          groupSettings={this.state.groupSettings}
          reload={this.reload}
          request={this.request}
          router={r}
          showProductImage={this.showProductImage} />
    })

    router.route('/households', _ => <HouseholdsPage households={this.state.households}
      groupSettings={this.state.groupSettings}
      request={this.request}
      reload={this.reload} />)

    router.route('/$', _ => <HouseholdsPage households={this.state.households}
      groupSettings={this.state.groupSettings}
      request={this.request}
      reload={this.reload} />)

    return (
      <div>
        <Icons className="hidden" />
        {!this.state.initialised ?
          <div className="fixed pin bg-grey-dark"></div>
          : !this.state.groupKey || !this.state.groupValid ?
            <div className="p-2">Group not found</div>
            : router.resolve()
        }
        <Loading loading={this.state.loading}></Loading>
        {!!this.state.error && (
          <div className="fixed pin p-2">
            <div className="mx-auto" style={{ maxWidth: 600 }}>
              <a href="#" onClick={e => { e.preventDefault(); this.setState({ error: null }); }} className="shadow-md border block text-black hover:text-black no-underline hover:no-underline relative p-2 bg-white flex mx-2">
                <Icon type="error" className="flex-no-shrink w-4 h-4 fill-current mr-2 nudge-d-2" />
                <span>{this.state.error.error}: {this.state.error.message}</span>
                <Icon type="close" className="flex-no-shrink ml-2 w-3 h-3 fill-current" />
              </a>
            </div>
          </div>
        )}
        {!!this.state.productImage && (
          <div className="fixed pin bg-black-translucent flex justify-center items-center" onClick={_ => this.setState({ productImage: null })}>
            <div className="w-full h-full p-4" style={{ maxWidth: 600 }}>
              <div className="bg-white rounded-sm w-full h-full shadow-md border flex flex-col">
                <a href="#"
                  onClick={e => { e.preventDefault(); this.setState({ productImage: null }); }}
                  className="block h-full p-4 relative text-black hover:text-black no-underline hover:no-underline flex flex-col">
                  <Icon type="close" className="absolute pin-t pin-r mt-2 mr-2 w-3 h-3 fill-current" />
                  <h2 className="text-md mt-4 text-center">{this.state.productTitle}</h2>
                  <div className="h-full flex flex-col justify-center items-center">
                    <img src={ServerApi.url.productImageFull(this.state.productImage)} />
                  </div>
                </a>
                {this.state.productUrl && 
                  <a href={this.state.productUrl} target="_blank" className="block p-4 text-center">See more info on the Suma website</a>
                }
              </div>
            </div>
          </div>
        )}
      </div>
    )
  }
}