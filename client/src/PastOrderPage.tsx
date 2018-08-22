import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'

export interface PastOrderPageProps { order: CollectiveOrder
                                    , householdOrders: HouseholdOrder[]
                                    }

export class PastOrderPage extends React.Component<PastOrderPageProps, {}> {
  render() {
    const order = this.props.order

    return (
      <div>
        <div><RouterLink path="/orders">Orders</RouterLink> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)} {order.isCancelled && ' (cancelled)'}</h1>
        <h2>Households</h2>
        <div>
          {this.props.householdOrders.map(h => (
            <div key={h.householdId}>
              <span>{h.householdName}</span>
              <Money amount={h.total} />
              <span>{h.isCancelled && 'cancelled'}</span>
              <RouterLink path={`/orders/${h.orderId}/households/${h.householdId}`}>View</RouterLink>
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