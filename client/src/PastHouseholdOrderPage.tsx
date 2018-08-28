import * as React from 'react';

import { HouseholdOrder, Product, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'

export interface PastHouseholdOrderPageProps { householdOrder: HouseholdOrder
                                             , referrer: 'order' | 'household'
                                             }

export class PastHouseholdOrderPage extends React.Component<PastHouseholdOrderPageProps, {}> {
  render() {
    const householdOrder = this.props.householdOrder

    return (
      <div>
        {this.props.referrer == 'order'
        ? (
          <div>
            <div>
              <RouterLink path="/orders">Orders</RouterLink> &gt;
              <RouterLink path={`/orders/${householdOrder.orderId}`}>{Util.formatDate(householdOrder.orderCreatedDate)}</RouterLink> &gt;
            </div>
            <h1>{householdOrder.householdName}</h1>
            <div>Status: {householdOrder.status}</div>
            <RouterLink path={`/households/${householdOrder.householdId}`}>View household</RouterLink>
          </div>
        )
        : (
          <div>
            <div>
              <RouterLink path="/households">Households</RouterLink> &gt;
              <RouterLink path={`/households/${householdOrder.householdId}`}>{householdOrder.householdName}</RouterLink> &gt;
            </div>
            <h1>{Util.formatDate(householdOrder.orderCreatedDate)}</h1>
            <div>Status: {householdOrder.status}</div>
            <RouterLink path={`/orders/${householdOrder.orderId}`}>View order</RouterLink>
          </div>
        )}
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