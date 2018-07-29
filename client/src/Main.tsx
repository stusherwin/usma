import * as React from 'react';

import { Order } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

export interface MainProps {}
export interface MainState { initialDataLoaded: boolean
                           , orders: Order[]
                           , error: ApiError | null
                           }

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    this.state = { initialDataLoaded: false
                 , orders: []
                 , error: null
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getOrders()])
      .then(results => {
        let orders = results[0]

        this.setState({ orders
                      , initialDataLoaded: true
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ error: apiError
                      })
      })
  }

  render() {
    const urlParts = window.location.href.split('/').filter(l => l.length).slice(2)

    switch(urlParts[0]) {
      case 'orders': return <OrdersPage orders={ this.state.orders } />
      case 'products': return <ProductsPage />
      case 'households': return <HouseholdsPage />
      default: return <div>Page not found</div>
    }
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

const ProductsPage = (props: {}) => {
  return (
    <div>
      <h1>Products</h1>
    </div>
  )
}

const HouseholdsPage = (props: {}) => {
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