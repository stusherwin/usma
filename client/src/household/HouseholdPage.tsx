import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { CurrentOrder } from './CurrentOrder'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'

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
    const order = <CurrentOrder household={this.props.household}
                                currentOrder={this.props.currentHouseholdOrder}
                                currentCollectiveOrder={this.props.currentCollectiveOrder}
                                products={this.props.products}
                                loading={this.props.loading}
                                request={this.props.request}
                                reload={this.props.reload} />

    // this.props.router.route('/order', _ => order)
    this.props.router.route('/past-orders', _ =>
      <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                           request={this.props.request}
                           reload={this.props.reload} />
    )
    this.props.router.route('/payments', _ => 
      <HouseholdPayments household={this.props.household}
                         payments={this.props.payments}
                         request={this.props.request}
                         reload={this.props.reload} />

    )
    this.props.router.route('/', _ => order)

    let basePath = `/households/${this.props.household.id}/`

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
        <nav className="flex justify-between bg-household-light">
          <RouterLink path={basePath}
                      className={classNames('p-2 text-center flex-grow rounded-t-lg bg-order-dark text-black ml-2 mr-1', { 
                        'bg-order-dark text-black': Router.isCurrent(basePath),
                        'hover:bg-order-dark hover:text-black': !Router.isCurrent(basePath)
                      })}>Current order</RouterLink>
          <RouterLink path={`${basePath}past-orders`}
                      className={classNames('p-2 text-center flex-grow rounded-t-lg bg-grey-lighter text-grey-darkest mr-1', { 
                        'bg-grey-lighter text-grey-darkest': Router.isCurrent(`${basePath}past-orders`),
                        'hover:bg-grey-lighter hover:text-grey-darkest': !Router.isCurrent(`${basePath}past-orders`) 
                      })}>Past orders</RouterLink>
          <RouterLink path={`${basePath}payments`} 
                      className={classNames('p-2 text-center flex-grow rounded-t-lg bg-payment-light text-payment-dark mr-2', { 
                        'bg-payment-light text-payment-dark': Router.isCurrent(`${basePath}payments`),
                        'hover:bg-payment-light hover:text-payment-dark': !Router.isCurrent(`${basePath}payments`) 
                      })}>Payments</RouterLink>
        </nav>
        {this.props.router.resolve()}
      </div>
    )
  }
}