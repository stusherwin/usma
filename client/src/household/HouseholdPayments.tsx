import * as React from 'react';

import { Household, HouseholdPayment } from '../Types'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , basePath: string
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface HouseholdPaymentsState { 
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, HouseholdPaymentsState> {
  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)
 
    return (
      <div>
        <RouterLink path={this.props.basePath} className="bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark">
          <Icon type="collapse" className="w-4 h-4 fill-current absolute pin-r mr-2" />
          <div className="bg-img-payment bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-payment-dark leading-none mb-2 -mt-1">Payments</h2>
            <h3 className="mt-0 flex justify-between"><span>Total payments:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
          </div>
        </RouterLink>
        {!this.props.payments.length &&
          <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
        }
        {!!this.props.payments.length &&
          <table className="border-collapse w-full mb-4">
            <tbody>
              { this.props.payments.map(p =>
                <tr key={p.id}>
                  <td className="pt-2 pl-2 pr-2 w-full">{ Util.formatDate(p.date) }</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={p.amount} /></td>
                </tr>
              ) }
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total payments</td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={total} /></td>
              </tr>
            </tbody>
          </table>
        }
      </div>
    )
  }
}