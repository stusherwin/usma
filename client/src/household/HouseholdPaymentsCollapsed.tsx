import * as React from 'react';

import { Household } from '../Types'
import { RouterLink } from '../common/RouterLink'
import { Money } from '../common/Money'
import { Icon } from '../common/Icon'

export interface HouseholdPaymentsCollapsedProps { household: Household
                                                 , basePath: string
                                                 }

export class HouseholdPaymentsCollapsed extends React.Component<HouseholdPaymentsCollapsedProps, {}> {
  render() {
    return (
      <RouterLink path={`${this.props.basePath}payments`} className="bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark">
        <Icon type="expand" className="w-4 h-4 fill-current absolute pin-r mr-2" />
        <div className="bg-img-payment bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
          <h2 className="text-payment-dark leading-none mb-2 -mt-1">Payments</h2>
          <h3 className="mt-0 flex justify-between"><span>Total payments:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
        </div>
      </RouterLink>
    )
  }
}