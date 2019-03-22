import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder, Household, PastHouseholdOrder } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { TopNav } from '../TopNav'

export interface PastOrderPageProps { order: PastCollectiveOrder
                                    , householdOrders: PastHouseholdOrder[]
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
            <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(order.createdDate)}</span><span><Money amount={order.totalIncVat} /></span></h3>
            <h3 className="font-normal">{order.isAbandoned? 'Abandoned' : ''}</h3>
          </div>
        </div>
        {!!this.props.householdOrders.length && 
          <table className="border-collapse w-full mb-4">
            <tbody>
              {this.props.householdOrders.map(ho => (
                <tr key={ho.householdId} className={classNames({'crossed-out': ho.isAbandoned})}>
                  <td className="pt-2 pl-2 pr-2"><RouterLink path={`/admin/orders/${ho.orderId}/households/${ho.householdId}`}>{ho.householdName}</RouterLink></td>
                  <td className="pt-2 pr-2">{ho.isAbandoned && 'Abandoned'}</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={ho.totalIncVat} /></td>
                </tr>
              ))}
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={order.totalIncVat} /></td>
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