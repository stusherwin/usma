import * as React from 'react';

import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'

import { OrdersPage } from './OrdersPage'
import { ProductsPage } from './ProductsPage'
import { HouseholdsPage } from './HouseholdsPage'

export interface MainProps {}
export interface MainState { loading: boolean
                           , error: ApiError | null
                           , url: string
                           }

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    const url = '/' + window.location.href.split('/').filter(l => l.length).slice(2).join('/')

    this.state = { loading: false
                 , error: null
                 , url
                 }
  }

  componentDidMount() {
    window.onpopstate = e => this.setState({url: e.state})
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
    let urlParts = this.state.url.split('/').slice(1)
    let location = urlParts[0]

    switch(location) {
      case 'orders': return <OrdersPage request={this.request} urlParts={urlParts.slice(1)} navigate={this.navigate} />
      case 'products': return <ProductsPage request={this.request} navigate={this.navigate} />
      case 'households': return <HouseholdsPage request={this.request} navigate={this.navigate} />
      default: return <div>Page not found</div>
    }
  }
}