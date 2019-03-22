import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder, Product, OrderItem } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { TopNav } from '../TopNav'

export interface PastHouseholdOrderPageProps { householdOrder: PastHouseholdOrder
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
          <div className="bg-img-household-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">{householdOrder.householdName}{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <div className="mt-6">
              <RouterLink path={`/admin/households/${householdOrder.householdId}`}>View household</RouterLink>
            </div>
          </div>
        </div>
        <div className="bg-grey-lighter p-2">
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past order</h2>
            <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(householdOrder.orderCreatedDate)}</span><span><Money amount={householdOrder.totalIncVat} /></span></h3>
            <h3 className="font-normal">{householdOrder.isAbandoned ? 'Abandoned' : ''}</h3>
            <div className="mt-2">
              <RouterLink path={`/admin/orders/${householdOrder.orderId}`}>View order</RouterLink>
            </div>
          </div>
        </div>
        {!!householdOrder.items.length && 
          <table className="border-collapse w-full mb-4">
            <tbody>
              {householdOrder.items.map(i => (
                  <tr key={i.productId}>  
                    <td className="pt-2 pl-2 pr-2">{i.productCode}</td>
                    <td className="pt-2 pr-2 w-full">{i.productName}</td>
                    <td className="pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                    <td className="pt-2 pr-2 text-right"><Money amount={i.itemTotalExcVat} /></td>
                  </tr>
              ))}
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2"></td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={householdOrder.totalIncVat} /></td>
              </tr>
            </tbody>
          </table>
        }
        {!householdOrder.items.length && 
          <div className="p-2  mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There were no order items in this order</div>
        }
      </div>
    )
  }
}