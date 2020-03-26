import * as React from 'react';

import { Household, CollectiveOrder, ProductCatalogueEntry, GroupSettings } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Money, Balance } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'

import { CollectiveOrderDetails } from 'household/CollectiveOrderDetails'
import { PastHouseholdOrders } from 'household/PastHouseholdOrders'
import { HouseholdPayments } from 'household/HouseholdPayments'
import { EditHousehold } from 'household/EditHousehold'

import { AdminTopNav } from 'admin/AdminTopNav'

export interface AdminHouseholdPageProps { household: Household
                                           collectiveOrder: CollectiveOrder | undefined
                                           products: ProductCatalogueEntry[]
                                           households: Household[]
                                           categories: string[]
                                           brands: string[]
                                           groupSettings: GroupSettings
                                           request: <T extends {}>(p: Promise<T>) => Promise<T>
                                           reload: () => Promise<void>
                                         }
export interface AdminHouseholdPageState { collapsibleState: CollapsibleState
                                         }

export class AdminHouseholdPage extends React.Component<AdminHouseholdPageProps, AdminHouseholdPageState> {
  editHousehold: React.RefObject<EditHousehold>

  constructor(props: AdminHouseholdPageProps) {
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

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        <AdminTopNav />
        <Collapsible collapsibleKey="household"
                     collapsibleState={this.state.collapsibleState}
                     onExpand={() => { if(this.editHousehold.current) { this.editHousehold.current.reset() } }}
                     onCollapse={() => { if(this.editHousehold.current) { this.editHousehold.current.blur() } }}
                     onExpanded={() => { if(this.editHousehold.current) { this.editHousehold.current.focus() } }}
                     header={
                       <div className="p-2 pt-0 pb-4 bg-household-light min-h-28">
                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                         <div className="ml-20 mt-2 flex items-start items-baseline justify-between">
                           <h2 className="mt-1 leading-none mr-2 mb-4">
                             {this.props.household.name}
                           </h2>
                           {this.props.groupSettings.enablePayments && 
                             <Balance className="bg-household-lighter ml-auto" amount={-this.props.household.balance} />
                           }
                         </div>
                         <div className="ml-20 text-base"><strong>Contact:</strong> {this.props.household.contactName || 'none'}</div>
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
        {this.props.groupSettings.enablePayments && 
          <HouseholdPayments collapsibleKey="payments"
                             collapsibleState={this.state.collapsibleState}
                             {...this.props} />
        }
        {this.props.groupSettings.enablePayments && 
          <div className="p-2 pl-20 text-black relative mt-2">
            <h3 className="mt-0 ml-2 flex justify-between">
              <span className="border-t-2 border-b-2 border-household-light pt-1 pb-1">Balance:</span>
              <Money className="text-right border-t-2 border-b-2 border-black pt-1 pb-1" amount={-this.props.household.balance} noColour />
            </h3>
          </div>
        }
      </div>
    )
  }
}