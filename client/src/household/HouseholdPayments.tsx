import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdPayment } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { CollapsibleWithHeader } from './CollapsibleWithHeader'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , expanded: boolean
                                        , otherExpanding: boolean
                                        , toggle: () => void
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, {}> {
  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)
 
    return (
      <CollapsibleWithHeader className="min-h-20"
                             headerClassName="bg-payment-light min-h-20"
                             headerImageClassName="bg-img-payment"
                             headerText="Payments"
                             headerContent={() => (
                               <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
                             )}
                             {...this.props}>
          { this.props.payments.length 
            ? <table className="border-collapse w-full">
                <tbody>
                  { this.props.payments.map((p, i) =>
                    <tr key={p.id}>
                      <td className={classNames('pr-2 w-full', {'pt-2': i > 0})}>{ Util.formatDate(p.date) }</td>
                      <td className={classNames('text-right whitespace-no-wrap', {'pt-2': i > 0})}><Money amount={p.amount} /></td>
                    </tr>
                  ) }
                  <tr>
                    <td className="pt-2 pr-2 font-bold">Total:</td>
                    <td className="pt-2 font-bold text-right whitespace-no-wrap"><Money amount={total} /></td>
                  </tr>
                </tbody>
              </table>
            : <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
          }
      </CollapsibleWithHeader>
    )
  }
}