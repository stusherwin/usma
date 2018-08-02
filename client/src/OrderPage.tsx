import * as React from 'react';

import { OrderSummary } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'
import { OrderHouseholdPage } from './OrderHouseholdPage'
import { OrderDetails } from './OrderDetails'

export interface OrderPageProps { id: number
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                , urlParts: string[]
                                }

export class OrderPage extends React.Component<OrderPageProps, {}> {
  render() {
    console.log(this.props.urlParts)
    if(this.props.urlParts[0] == 'households') {
      let householdId = parseInt(this.props.urlParts[1]) || null
      if(householdId) return <OrderHouseholdPage orderId={this.props.id} householdId={householdId} request={this.props.request} navigate={this.props.navigate} />
    }

    return <OrderDetails id={this.props.id} request={this.props.request} navigate={this.props.navigate} />
  }
}