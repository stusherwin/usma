import * as React from 'react';

import { HouseholdOrder, Product, OrderItem } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Money } from '../Money'

export interface PastHouseholdOrderPageProps { householdOrder: HouseholdOrder
                                             , referrer: 'order' | 'household'
                                             , loading: boolean
                                             , error: ApiError | null
                                             }

export class PastHouseholdOrderPage extends React.Component<PastHouseholdOrderPageProps, {}> {
  render() {
    const householdOrder = this.props.householdOrder

    return (
      <div>
        <div hidden={!this.props.loading}>Loading...</div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-img-household bg-no-repeat bg-16 pl-16 min-h-16 bg-household-light">
          <div>
            <RouterLink path="/orders">Orders</RouterLink>
            <RouterLink path="/products">Products</RouterLink>
            <RouterLink path="/households">Households</RouterLink>
          </div>
          <div>
            <div>
              <RouterLink path="/households">Households</RouterLink> &gt;
              <RouterLink path={`/households/${householdOrder.householdId}`}>{householdOrder.householdName}</RouterLink> &gt;
            </div>
            <h1>{Util.formatDate(householdOrder.orderCreatedDate)}</h1>
            <div>Status: {householdOrder.status}</div>
          </div>
        </div>
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