import * as React from 'react';

import { Household, HouseholdOrder, CollectiveOrder, HouseholdPayment } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'
import { Router } from './Router'
import { HouseholdOrders } from './HouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'

export interface HouseholdOrdersPageProps { household: Household
                                          , householdOrders: HouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | undefined
                                          , payments: HouseholdPayment[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, {}> {
  render() {
    return (
      <div>
        <div><RouterLink path="/households">Households</RouterLink> &gt;</div>
        <h1>{this.props.household.name}</h1>
        <div>Total orders: <Money amount={this.props.household.totalOrders} /></div>
        <div>Total payments: <Money amount={this.props.household.totalPayments} /></div>
        <div>Balance: <Money amount={this.props.household.balance} /></div>
        <div>
          <HouseholdOrders household={this.props.household}
                           householdOrders={this.props.householdOrders}
                           currentCollectiveOrder={this.props.currentCollectiveOrder}
                           request={this.props.request}
                           reload={this.props.reload} />
          <HouseholdPayments household={this.props.household}
                             payments={this.props.payments}
                             request={this.props.request}
                             reload={this.props.reload} />
        </div>
      </div>
    )
  }
}