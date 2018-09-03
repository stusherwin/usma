import * as React from 'react';
import * as classNames from 'classnames'

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
      <div>
        <div className="bg-grey-lighter p-2">
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2 overflow-auto">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past orders</h2>
          </div>
        </div>
        {!pastOrders.length
        ? <div className="p-2">No past orders</div>
        : (
          <table className="border-collapse w-full mb-4">
            { pastOrders.map(ho => (
              <tr key={ho.orderId} className={classNames({'crossed-out': ho.status == 'Cancelled'})}>
                <td className="pt-2 pl-2 pr-2"><RouterLink path={`/households/${ho.householdId}/orders/${ho.orderId}`}>{Util.formatDate(ho.orderCreatedDate) }</RouterLink></td>
                <td className="pt-2 pr-2">{ho.status}</td>
                <td className="pt-2 pr-2 text-right"><Money amount={ho.total} /></td>
              </tr>
            )) }
            {!!pastOrders.length &&
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 text-right font-bold"><Money amount={total} /></td>
              </tr>
            }
          </table>
        )}
      </div>
    )
  }
}