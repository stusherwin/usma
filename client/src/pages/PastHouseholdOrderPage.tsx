import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, Product, OrderItem } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Icon } from '../Icon'
import { Money } from '../Money'
import { TopNav } from '../TopNav'

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
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-grey-light p-2">
          <TopNav className="text-grey-darkest hover:text-black" />
          <div className="bg-img-household-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4 overflow-auto">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">{householdOrder.householdName}{!!this.props.loading && <Icon type="refresh" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <div className="mt-6">
              <RouterLink path={`/households/${householdOrder.householdId}`}>View household</RouterLink>
            </div>
          </div>
        </div>
        <div className="bg-grey-lighter p-2">
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2 overflow-auto">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past order</h2>
            <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(householdOrder.orderCreatedDate)}</span><span><Money amount={householdOrder.total} /></span></h3>
            <h3 className="font-normal">{householdOrder.status}</h3>
            <div className="mt-2">
              <RouterLink path={`/orders/${householdOrder.orderId}`}>View order</RouterLink>
            </div>
          </div>
        </div>
        <table className="border-collapse w-full mb-4">
          {householdOrder.items.map(i => (
              <tr key={i.productId}>
                <td className="pt-2 pl-2 pr-2 w-full">{i.productName}</td>
                <td className="pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                <td className="pt-2 pr-2 text-right"><Money amount={i.itemTotal} /></td>
              </tr>
          ))}
          {!!householdOrder.items.length && 
            <tr>
              <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
              <td className="pt-2 pr-2"></td>
              <td className="pt-2 pr-2 font-bold text-right"><Money amount={householdOrder.total} /></td>
            </tr>
          }
        </table>
      </div>
    )
  }
}