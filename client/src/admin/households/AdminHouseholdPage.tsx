import * as React from 'react';

import { Household, CollectiveOrder, ProductCatalogueEntry } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Money } from '../../util/Money'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { CollectiveOrderDetails } from '../../household/CollectiveOrderDetails'
import { PastHouseholdOrders } from '../../household/PastHouseholdOrders'
import { HouseholdPayments } from '../../household/HouseholdPayments'
import { EditHousehold } from '../../household/EditHousehold'

import { AdminTopNav } from '../AdminTopNav'

export interface AdminHouseholdOrdersPageProps { household: Household
                                               , collectiveOrder: CollectiveOrder | null
                                               , products: ProductCatalogueEntry[]
                                               , households: Household[]
                                               , categories: string[]
                                               , brands: string[]
                                               , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                               , reload: () => Promise<void>
                                               }
export interface AdminHouseholdOrdersPageState { collapsibleState: CollapsibleState
                                               }

export class AdminHouseholdPage extends React.Component<AdminHouseholdOrdersPageProps, AdminHouseholdOrdersPageState> {
  editHousehold: React.RefObject<EditHousehold>

  constructor(props: AdminHouseholdOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('orders', collapsibleState => this.setState({collapsibleState})) 
    }

    this.editHousehold = React.createRef();
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
                     collapsibleKey="household"
                     collapsibleState={this.state.collapsibleState}
                     onExpand={() => { if(this.editHousehold.current) { this.editHousehold.current.reset() } }}
                     onCollapse={() => { if(this.editHousehold.current) { this.editHousehold.current.blur() } }}
                     onExpanded={() => { if(this.editHousehold.current) { this.editHousehold.current.focus() } }}
                     header={
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
                         onConfirm={() => this.props.reload().then(this.state.collapsibleState.toggle('household'))}
                         onCancel={this.state.collapsibleState.toggle('household')}
                         {...this.props} />
        </Collapsible>
        <CollectiveOrderDetails collapsibleKey="orders"
                                collapsibleState={this.state.collapsibleState}
                                {...this.props} />
        <PastHouseholdOrders collapsibleKey="past-orders"
                             collapsibleState={this.state.collapsibleState}
                             {...this.props} />
        <HouseholdPayments collapsibleKey="payments"
                           collapsibleState={this.state.collapsibleState}
                           {...this.props} />
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