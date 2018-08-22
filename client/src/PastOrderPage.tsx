import * as React from 'react';

import { CollectiveOrder, Household, HouseholdOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface PastOrderPageProps { order: CollectiveOrder
                                    , householdOrders: HouseholdOrder[]
                                    , navigate: (location: string) => void
                                    }

export class PastOrderPage extends React.Component<PastOrderPageProps, {}> {
  render() {
    const order = this.props.order

    return (
      <div>
        <div><Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;</div>
        <h1>{Util.formatDate(order.createdDate)} {order.isCancelled && ' (cancelled)'}</h1>
        <div>
          {this.props.householdOrders.map(h => (
            <div key={h.householdId}>
              <span>{h.householdName}</span>
              <Money amount={h.total} />
              <span>{h.isCancelled && 'cancelled'}</span>
              <Link action={_ => this.props.navigate(`/orders/${h.orderId}/households/${h.householdId}`)}>View</Link>
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