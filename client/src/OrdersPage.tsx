import * as React from 'react';

import { Order } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

import { OrderPage } from './OrderPage'
import { OrderList } from './OrderList'

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