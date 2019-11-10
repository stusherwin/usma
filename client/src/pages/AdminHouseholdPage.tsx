import * as React from 'react';

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Money } from '../common/Money'
import { CurrentOrder } from '../components/CurrentOrderForHousehold'
import { PastHouseholdOrders } from '../components/PastHouseholdOrders'
import { HouseholdPayments } from '../components/HouseholdPayments'
import { AdminTopNav } from '../components/AdminTopNav'
import { Collapsible } from '../common/Collapsible'
import { EditHousehold } from '../components/EditHousehold'

export interface AdminHouseholdOrdersPageProps { household: Household
                                               , currentOrder: CollectiveOrder | null
                                               , currentHouseholdOrder: HouseholdOrder | null
                                               , currentHouseholdOrders: HouseholdOrder[]
                                               , pastHouseholdOrders: PastHouseholdOrder[]
                                               , payments: HouseholdPayment[]
                                               , products: ProductCatalogueEntry[]
                                               , categories: string[]
                                               , brands: string[]
                                               , households: Household[]
                                               , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                               , reload: () => Promise<void>
                                               }
type Section = 'orders' | 'past-orders' | 'payments' | 'household'
export interface AdminHouseholdOrdersPageState { expanded: Section | null
                                          }

export class AdminHouseholdPage extends React.Component<AdminHouseholdOrdersPageProps, AdminHouseholdOrdersPageState> {
  editHousehold: React.RefObject<EditHousehold>

  constructor(props: AdminHouseholdOrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'orders', 
    }

    this.editHousehold = React.createRef();
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = (orderId: number) => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }
  
  render() {
    return (
      <div className="bg-household-light min-h-screen">
        <AdminTopNav />
        <Collapsible className="min-h-24"
                     expanded={this.state.expanded == 'household'}
                     otherExpanding={!!this.state.expanded && this.state.expanded != 'household'}
                     toggle={this.toggle('household')}
                     onExpand={() => { if(this.editHousehold.current) { this.editHousehold.current.reset() } }}
                     onCollapse={() => { if(this.editHousehold.current) { this.editHousehold.current.blur() } }}
                     onExpanded={() => { if(this.editHousehold.current) { this.editHousehold.current.focus() } }}
                     header={() =>
                       <div className="p-2 bg-household-light min-h-24">
                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-household mt-2"></div>
                         <h2 className="leading-none ml-20 relative flex mt-2">
                           {this.props.household.name}
                         </h2>
                         <div>
                           <div className="ml-20 text-lg mt-4"><strong>Contact:</strong> {this.props.household.contactName || 'none'}</div>
                         </div>
                       </div>
                     }>
          <EditHousehold ref={this.editHousehold}
                         household={this.props.household}
                         request={this.props.request}
                         onConfirm={() => this.props.reload().then(this.toggle('household'))}
                         onCancel={this.toggle('household')} />
        </Collapsible>
        <CurrentOrder household={this.props.household}
                      currentOrder={this.props.currentOrder}
                      currentHouseholdOrder={this.props.currentHouseholdOrder}
                      currentHouseholdOrders={this.props.currentHouseholdOrders}
                      products={this.props.products}
                      categories={this.props.categories}
                      brands={this.props.brands}
                      households={this.props.households}
                      expanded={this.state.expanded == 'orders'}
                      otherExpanding={!!this.state.expanded && this.state.expanded != 'orders'}
                      toggle={this.toggle('orders')}
                      request={this.props.request}
                      reload={this.props.reload} />
        <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                             currentHouseholdOrder={this.props.currentHouseholdOrder}
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
        <div className="bg-household-light p-2 pl-20 text-black">
          <h3 className="mt-0 ml-2 flex justify-between">
            <span>Balance ({this.props.household.balance < 0? 'owing' : 'in credit' }):</span>
            <span className="text-right"><Money amount={this.props.household.balance} absolute /></span>
          </h3>
        </div>
      </div>
    )
  }
}