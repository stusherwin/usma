import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder } from '../../Types'
import { ServerApi, ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { Router } from '../../common/Router'

export interface PastHouseholdOrdersProps { householdOrders: PastHouseholdOrder[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, {}> {
  render() {
    const pastOrders = this.props.householdOrders
    const total = this.props.householdOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)

    return (
      <div>
        <div className="bg-grey-lighter p-2">
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past orders</h2>
          </div>
        </div>
        {!pastOrders.length
        ? <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders</div>
        : (
          <table className="border-collapse w-full mb-4">
            <tbody>
              { pastOrders.map(ho => (
                <tr key={ho.orderId} className={classNames({'crossed-out': ho.isAbandoned})}>
                  <td className="pt-2 pl-2 pr-2"><RouterLink path={`/admin/households/${ho.householdId}/orders/${ho.orderId}`}>{Util.formatDate(ho.orderCreatedDate) }</RouterLink></td>
                  <td className="pt-2 pr-2">{ho.isAbandoned && 'Abandoned'}</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={ho.totalIncVat} /></td>
                </tr>
              )) }
              {!!pastOrders.length &&
                <tr>
                  <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                  <td className="pt-2 pr-2"></td>
                  <td className="pt-2 pr-2 text-right font-bold"><Money amount={total} /></td>
                </tr>
              }
            </tbody>
          </table>
        )}
      </div>
    )
  }
}