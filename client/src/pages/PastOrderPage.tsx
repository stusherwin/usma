import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Money } from '../Money'
import { TopNav } from '../TopNav'

export interface PastOrderPageProps { order: CollectiveOrder
                                    , householdOrders: HouseholdOrder[]
                                    , loading: boolean
                                    , error: ApiError | null
                                    }

export class PastOrderPage extends React.Component<PastOrderPageProps, {}> {
  render() {
    const order = this.props.order

    return (
      <div>
        <div hidden={!this.props.loading}>Loading...</div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-img-order bg-no-repeat bg-16 pl-16 min-h-16 bg-order-dark">
          <TopNav />
          <div><RouterLink path="/orders">Orders</RouterLink> &gt;</div>
          <h1>{Util.formatDate(order.createdDate)}</h1>
          <div>Status: {order.status}</div>
        </div>
        <h2>Households</h2>
        <div>
          {this.props.householdOrders.map(ho => (
            <div key={ho.householdId}>
              <RouterLink path={`/orders/${ho.orderId}/households/${ho.householdId}`}>{ho.householdName}</RouterLink>
              <Money amount={ho.total} />
              <span>{ho.status}</span>
            </div>
          ))}
          <div>
            <span>Total:</span>
            <Money amount={order.total} />
          </div>
        </div>
      </div>
    )
  }
}