import * as React from 'react';

import { OrderPage } from './OrderPage'
import { OrderList } from './OrderList'

export interface OrdersPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , navigate: (location: string) => void
                                 , urlParts: string[]
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, {}> {
  render() {
    let orderId = parseInt(this.props.urlParts[0]) || null
    let urlRemainder = this.props.urlParts.slice(1)

    if(orderId) return <OrderPage id={orderId} request={this.props.request} navigate={this.props.navigate} urlParts={urlRemainder} />

    return (
      <div>
        <OrderList request={this.props.request} navigate={this.props.navigate} />
      </div>
    )
  }
}