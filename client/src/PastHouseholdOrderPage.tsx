import * as React from 'react';

import { HouseholdOrder, Product, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'

export interface PastHouseholdOrderPageProps { householdOrder: HouseholdOrder
                                             }

export class PastHouseholdOrderPage extends React.Component<PastHouseholdOrderPageProps, {}> {
  render() {
    const householdOrder = this.props.householdOrder

    return (
      <div>
        <div>
          <RouterLink path="/orders">Orders</RouterLink> &gt;
          <RouterLink path={`/orders/${householdOrder.orderId}`}>{Util.formatDate(householdOrder.orderCreatedDate)}</RouterLink> &gt;
        </div>
        <h1>{householdOrder.householdName} {householdOrder.isCancelled && ' (cancelled)'}</h1>
        <h2>Items</h2>
        <div>
          {householdOrder.items.map(i => (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>x {i.itemQuantity}</span>
              <Money amount={i.itemTotal} />
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={householdOrder.total} />
          </div>
        </div>
      </div>
    )
  }
}