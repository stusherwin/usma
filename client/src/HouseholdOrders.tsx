import * as React from 'react';

import { Household, HouseholdOrder, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'

export interface HouseholdOrdersProps { household: Household
                                      , householdOrders: HouseholdOrder[]
                                      , currentCollectiveOrder: CollectiveOrder | undefined
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, {}> {
  render() {
    const currentOrder = this.props.householdOrders.filter(ho => !ho.isOrderPast && !ho.isCancelled)[0]
    const pastOrders = this.props.householdOrders.filter(ho => ho.isOrderPast || ho.isCancelled)
    const currentCollectiveOrder = this.props.currentCollectiveOrder
    const total = this.props.householdOrders.filter(ho => !ho.isCancelled).reduce((tot, ho) => tot + ho.total, 0)

    return (
      <div  className="m-2 mt-4">
        <h2>Past orders</h2>
        {!pastOrders.length ? <div>No past orders</div> : (
          <div>
            { pastOrders.map(ho => (
              <div key={ho.orderId}>
                <RouterLink path={`/households/${ho.householdId}/orders/${ho.orderId}`}>{Util.formatDate(ho.orderCreatedDate) }</RouterLink>
                <span>{ho.status}</span>
                <Money amount={ho.total} />
              </div>
            )) }
          </div>
        )}
        <div>
          <span>Total:</span>
          <Money amount={total} />
        </div>
      </div>
    )
  }
}