import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { CurrentOrderCollapsed } from './CurrentOrderCollapsed'
import { CurrentOrder } from './CurrentOrder'
import { PastHouseholdOrdersCollapsed } from './PastHouseholdOrdersCollapsed'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'
import { HouseholdPaymentsCollapsed } from './HouseholdPaymentsCollapsed'

export interface HouseholdOrdersPageProps { household: Household
                                          , currentHouseholdOrder: HouseholdOrder | null
                                          , pastHouseholdOrders: PastHouseholdOrder[]
                                          , currentCollectiveOrder: CollectiveOrder | null
                                          , payments: HouseholdPayment[]
                                          , products: ProductCatalogueEntry[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          , loading: boolean
                                          , error: ApiError | null
                                          , router: Router
                                          }

export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, {}> {  
  render() {
    let basePath = `/households/${this.props.household.id}/`

    this.props.router.route('/order', _ =>
      <div>
        <CurrentOrder household={this.props.household}
                      currentOrder={this.props.currentHouseholdOrder}
                      currentCollectiveOrder={this.props.currentCollectiveOrder}
                      products={this.props.products}
                      loading={this.props.loading}
                      basePath={basePath}
                      request={this.props.request}
                      reload={this.props.reload} />
        <PastHouseholdOrdersCollapsed householdOrders={this.props.pastHouseholdOrders}
                                      basePath={basePath} />
        <HouseholdPaymentsCollapsed household={this.props.household}
                                    basePath={basePath} />
      </div>
    )
    this.props.router.route('/past-orders', _ =>
      <div>
        <CurrentOrderCollapsed household={this.props.household}
                               currentOrder={this.props.currentHouseholdOrder}
                               currentCollectiveOrder={this.props.currentCollectiveOrder}
                               basePath={basePath} />
        <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                             basePath={basePath}
                             request={this.props.request}
                             reload={this.props.reload} />
        <HouseholdPaymentsCollapsed household={this.props.household}
                                    basePath={basePath} />
      </div>
    )
    this.props.router.route('/payments', _ => 
      <div>
        <CurrentOrderCollapsed household={this.props.household}
                               currentOrder={this.props.currentHouseholdOrder}
                               currentCollectiveOrder={this.props.currentCollectiveOrder}
                               basePath={basePath} />
        <PastHouseholdOrdersCollapsed householdOrders={this.props.pastHouseholdOrders}
                                      basePath={basePath} />
        <HouseholdPayments household={this.props.household}
                           payments={this.props.payments}
                           basePath={basePath}
                           request={this.props.request}
                           reload={this.props.reload} />
      </div>
    )
    this.props.router.route('/', _ => 
      <div>
        <CurrentOrderCollapsed household={this.props.household}
                               currentOrder={this.props.currentHouseholdOrder}
                               currentCollectiveOrder={this.props.currentCollectiveOrder}
                               basePath={basePath} />
        <PastHouseholdOrdersCollapsed householdOrders={this.props.pastHouseholdOrders} 
                                      basePath={basePath}/>
        <HouseholdPaymentsCollapsed household={this.props.household}
                                    basePath={basePath} />
      </div>
    )

    return (
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-household-light p-2 pb-4">
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="leading-none mb-2 -mt-1 text-household-darker">{this.props.household.name}{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <table className="border-collapse w-full text-household-darker">
              <tbody>
                <tr>
                  <td>Current balance:</td>
                  <td className={classNames('text-right', 'font-bold', {'text-red-dark': this.props.household.balance < 0})}><Money amount={this.props.household.balance} /></td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
        {this.props.router.resolve()}
      </div>
    )
  }
}