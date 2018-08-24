import * as React from 'react';

import { Household, HouseholdPayment, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'
import { Router } from './Router'

export interface HouseholdPaymentsPageProps { household: Household
                                            , payments: HouseholdPayment[]
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => void
                                            }

export class HouseholdPaymentsPage extends React.Component<HouseholdPaymentsPageProps, {}> {
  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)

    return (
      <div>
        <div><RouterLink path="/households">Households</RouterLink> &gt;</div>
        <h1>{this.props.household.name}</h1>
        <div>
          <RouterLink path={`/households/${this.props.household.id}/orders`}>Orders</RouterLink>
          <div>Payments</div>
          
          {!this.props.payments.length 
          ? <div>No payments yet</div>
          : (
            <div>
              { this.props.payments.map(p => (
                <div key={p.id}>
                  <span>{ Util.formatDate(p.date) }</span>
                  <Money amount={p.amount} />
                </div>
              )) }
            </div>
          )}

          <div>
            <span>Total:</span>
            <Money amount={total} />
          </div>
        </div>
      </div>
    )
  }
}