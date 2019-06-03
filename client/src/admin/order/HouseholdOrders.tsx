import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../../Types'
import { ServerApi } from '../../ServerApi'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'

export interface HouseholdOrdersProps { order: CollectiveOrder
                                      , householdOrders: HouseholdOrder[]
                                      , households: Household[]
                                      , addingHousehold: Household | null
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, {}> {
  deleteHouseholdOrder = (h: HouseholdOrder) => {
    this.props.request(ServerApi.command.deleteHouseholdOrder(h.orderId, h.householdId))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.order
    const deletableHousehold = !!this.props.householdOrders.length && !!this.props.householdOrders.find(ho => !ho.items.length)

    return (
      <table className="border-collapse w-full">
        <tbody>
          {this.props.householdOrders.map(ho => {
            let household = this.props.households.find(h => h.id == ho.householdId)
            let status = (
              <span>
                {ho.status}
                {ho.isComplete &&
                  <span>
                    {household && (
                      household.balance <= 0
                      ? ' (paid)'
                      : (<span> (<Money amount={household.balance} absolute /> to pay <RouterLink path={`/admin/households/${ho.householdId}`}>Make payment</RouterLink>)</span>)
                    )}
                  </span>
                }
              </span>
            )
            return (
              <tr key={ho.householdId} className={classNames({'crossed-out': ho.status == 'Abandoned'})}>
                <td className="pt-4 pl-2 pr-2"><RouterLink path={`/admin/households/${ho.householdId}`}>{ho.householdName}</RouterLink></td>
                <td className="pt-4 pr-2">{status}</td>
                <td className="pt-4 pr-2 text-right"><Money amount={ho.totalIncVat} /></td>
                {deletableHousehold && 
                  <td className="pt-4 pr-2 w-1">
                    {!ho.items.length &&
                      <button disabled={!!this.props.addingHousehold} onClick={_ => this.deleteHouseholdOrder(ho)}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                    }
                  </td>
                }
              </tr>
            )}
          )}
          <tr>
            <td className="pt-4 pb-4 pl-2 pr-2 font-bold">Total</td>
            <td className="pt-4 pb-4 pr-2"></td>
            <td className="pt-4 pb-4 pr-2 font-bold text-right"><Money amount={order.totalIncVat} /></td>
            {deletableHousehold && 
              <td className="pt-4 pb-4 pr-2 w-1"><div className="bg-transparent w-9 h-6"></div></td>
            }
          </tr>
        </tbody>
      </table>
    )
  }
}