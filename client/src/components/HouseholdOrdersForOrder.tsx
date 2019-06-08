import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household, HouseholdOrder } from '../Types'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible } from '../common/Collapsible'
import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'

export interface HouseholdOrdersProps { order: CollectiveOrder
                                      , householdOrders: HouseholdOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

export interface HouseholdOrdersState { expanded: HouseholdOrder | null }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, HouseholdOrdersState> {
  constructor(props: HouseholdOrdersProps) {
    super(props)

    this.state = { 
      expanded: null, 
    }
  }

  toggle = (toExpand: HouseholdOrder) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    const order = this.props.order

    return (
      <table className="border-collapse w-full">
        <tbody>
          {this.props.householdOrders.map((ho, i) => {
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
              <tr key={ho.householdId}>
                <td colSpan={3}>
                  <Collapsible className="min-h-24"
                               expanded={this.state.expanded == ho}
                               otherExpanding={!!this.state.expanded && this.state.expanded != ho}
                               toggle={this.toggle(ho)}
                               header={() =>
                                 <div className={classNames('p-2 bg-household-lighter min-h-24')}>
                                   <div className="bg-no-repeat w-16 h-16 absolute bg-img-household mt-2"></div>
                                   <h3 className="leading-none ml-20 relative flex mt-2">
                                     {ho.householdName}
                                   </h3>
                                   <h4 className="flex justify-end ml-20 mt-4 mb-4">
                                     <span>Total:</span>
                                     <span className="w-24 font-bold text-right"><Money amount={ho.totalIncVat} /></span>
                                   </h4>
                                 </div>
                               }>
                    {!ho.items.length?
                      <div className="shadow-inner-top px-2 py-4 bg-white text-grey-darker">
                        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet
                      </div>
                    : <div className="shadow-inner-top px-2 py-4 bg-white text-grey-darker">
                        <CurrentHouseholdOrder currentHouseholdOrder={ho}
                                               reload={this.props.reload}
                                               request={this.props.request}
                                               readOnly={true} />
                      </div>
                    }
                  </Collapsible>
                </td>
              </tr>
            )}
          )}
          <tr>
            <td className="pt-4 pb-4 pl-2 pr-2 font-bold" colSpan={3}>
              <div className="flex justify-end">
                <span>Total:</span>
                <span className="w-24 font-bold text-right"><Money amount={order.totalIncVat} /></span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    )
  }
}