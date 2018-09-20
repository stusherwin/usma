import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Icon } from '../Icon'
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
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-grey-lighter p-2">
          <TopNav className="text-grey-darkest hover:text-black" />
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past order{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(order.createdDate)}</span><span><Money amount={order.total} /></span></h3>
            <h3 className="font-normal">{order.status}</h3>
          </div>
        </div>
        {!!this.props.householdOrders.length && 
          <table className="border-collapse w-full mb-4">
            <tbody>
              {this.props.householdOrders.map(ho => (
                <tr key={ho.householdId} className={classNames({'crossed-out': ho.status == 'Cancelled'})}>
                  <td className="pt-2 pl-2 pr-2"><RouterLink path={`/orders/${ho.orderId}/households/${ho.householdId}`}>{ho.householdName}</RouterLink></td>
                  <td className="pt-2 pr-2">{ho.status}</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={ho.total} /></td>
                </tr>
              ))}
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={order.total} /></td>
              </tr>
            </tbody>
          </table>
        }
        {!this.props.householdOrders.length && 
          <div className="p-2 mb-4">There were no households taking part in this order</div>
        }
      </div>
    )
  }
}