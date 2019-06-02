import * as React from 'react';

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { CurrentOrder } from './CurrentOrder'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'
import { ApiError } from '../ServerApi'
import { CollapsibleWithHeader } from './CollapsibleWithHeader'
import { RouterLink } from '../common/RouterLink'
import { EditHousehold } from './EditHousehold'
import { Loading } from './Loading'

export interface HouseholdOrdersPageProps { household: Household
                                          , currentOrder: CollectiveOrder | null
                                          , currentHouseholdOrder: HouseholdOrder | null
                                          , currentHouseholdOrders: HouseholdOrder[]
                                          , pastHouseholdOrders: PastHouseholdOrder[]
                                          , payments: HouseholdPayment[]
                                          , products: ProductCatalogueEntry[]
                                          , households: Household[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          , loading: boolean
                                          , error: ApiError | null
                                          , router: Router
                                          }
type Section = 'orders' | 'past-orders' | 'payments' | 'household'
export interface HouseholdOrdersPageState { expanded: Section | null
                                          }
export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, HouseholdOrdersPageState> {  
  editHousehold: React.RefObject<EditHousehold>

  constructor(props: HouseholdOrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'orders', 
    }

    this.editHousehold = React.createRef();
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <CollapsibleWithHeader className="min-h-28"
                               headerClassName="bg-household-light min-h-28"
                               headingClassName="mt-2"
                               headerImageClassName="bg-img-household mt-2"
                               headerText={this.props.household.name}
                               headerContent={() => (
                                 <div>
                                   <div className="ml-20 mt-1">
                                     <RouterLink path="/households">Change household</RouterLink>
                                   </div>
                                   <div className="ml-20 text-lg mt-4"><strong>Contact:</strong> {this.props.household.contactName || 'none'}</div>
                                 </div>
                               )}
                               expanded={this.state.expanded == 'household'}
                               otherExpanding={!!this.state.expanded && this.state.expanded != 'household'}
                               toggle={this.toggle('household')}
                               onExpand={() => { if(this.editHousehold.current) { this.editHousehold.current.reset() } }}
                               onCollapse={() => { if(this.editHousehold.current) { this.editHousehold.current.blur() } }}
                               onExpanded={() => { if(this.editHousehold.current) { this.editHousehold.current.focus() } }}>
          <EditHousehold ref={this.editHousehold}
                         household={this.props.household}
                         request={this.props.request}
                         onConfirm={() => this.props.reload().then(this.toggle('household'))}
                         onCancel={this.toggle('household')}
            >
          </EditHousehold>
        </CollapsibleWithHeader>
        <CurrentOrder household={this.props.household}
                      currentOrder={this.props.currentOrder}
                      currentHouseholdOrder={this.props.currentHouseholdOrder}
                      currentHouseholdOrders={this.props.currentHouseholdOrders}
                      products={this.props.products}
                      households={this.props.households}
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
                           editable={false}
                           otherExpanding={!!this.state.expanded && this.state.expanded != 'payments'}
                           toggle={this.toggle('payments')}
                           request={this.props.request}
                           reload={this.props.reload} />
        <div className="bg-household-light p-2 pl-20 text-black">
          <h3 className="mt-0 ml-2 flex justify-between"><span>Balance:</span><span><Money amount={-this.props.household.balance} /></span></h3>
        </div>
        <Loading loading={this.props.loading}></Loading>
      </div>
    )
  }
}