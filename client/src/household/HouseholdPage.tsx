import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { ApiError } from '../ServerApi'
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
type Section = 'orders' | 'past-orders' | 'payments'
export interface HouseholdOrdersPageState { expanded: Section | null }
export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, HouseholdOrdersPageState> {  
  constructor(props: HouseholdOrdersPageProps) {
    super(props)
    this.state = { expanded: 'orders' }
  }

  toggle = (toExpand: Section) => () => this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}))

  render() {
    return (
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-household-light p-2 pt-4 pb-4">
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative">
            <h2 className="leading-none text-household-darker">{this.props.household.name}{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
          </div>
        </div>
        <CurrentOrder household={this.props.household}
            currentOrder={this.props.currentHouseholdOrder}
            currentCollectiveOrder={this.props.currentCollectiveOrder}
            products={this.props.products}
            loading={this.props.loading}
            expanded={this.state.expanded == 'orders'}
            otherExpanding={!!this.state.expanded && this.state.expanded != 'orders'}
            toggle={this.toggle('orders')}
            request={this.props.request}
            reload={this.props.reload} />
        <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                             expanded={this.state.expanded == 'past-orders'}
                             otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders'}
                             toggle={this.toggle('past-orders')}
                             request={this.props.request}
                             reload={this.props.reload} />
        <HouseholdPayments household={this.props.household}
                           payments={this.props.payments}
                           expanded={this.state.expanded == 'payments'}
                           otherExpanding={!!this.state.expanded && this.state.expanded != 'payments'}
                           toggle={this.toggle('payments')}
                           request={this.props.request}
                           reload={this.props.reload} />
        <div className="bg-household-light p-2 pl-20 text-household-darker">
          <h3 className="mt-0 ml-2 flex justify-between"><span>Balance:</span><span><Money amount={this.props.household.balance} /></span></h3>
        </div>
      </div>
    )
  }
}