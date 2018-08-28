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
        <h1>{Util.formatDate(order.createdDate)}</h1>
        <div>Status: {order.status}</div>
        <h2>Households</h2>
        <div>
          {this.props.householdOrders.map(ho => (
            <div key={ho.householdId}>
              <span>{ho.householdName}</span>
              <Money amount={ho.total} />
              <span>{ho.status}</span>
              <RouterLink path={`/orders/${ho.orderId}/households/${ho.householdId}`}>View</RouterLink>
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