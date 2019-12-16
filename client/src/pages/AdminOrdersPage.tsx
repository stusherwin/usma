import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder, PastHouseholdOrder } from '../Types'
import { AdminTopNav } from '../components/AdminTopNav'
import { CurrentOrder } from '../components/CurrentOrder'
import { PastOrders } from '../components/PastOrders'
import { CollapsibleState } from '../common/Collapsible'

export interface AdminOrdersPageProps { currentOrder: CollectiveOrder | null
                                      , currentHouseholdOrders: HouseholdOrder[]
                                      , pastOrders: PastCollectiveOrder[]
                                      , pastHouseholdOrders: PastHouseholdOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }
type Section = 'order' | 'past-orders'
export interface AdminOrdersPageState { collapsibleState: CollapsibleState }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('order', collapsibleState => {console.log('hi'); this.setState({collapsibleState})})
    }
  }

  render() {
    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <CurrentOrder collapsibleKey="order"
                      collapsibleState={this.state.collapsibleState}
                      {...this.props} />
        <PastOrders collapsibleKey="past-orders"
                    collapsibleState={this.state.collapsibleState}
                    {...this.props} />
      </div>
    )
  }
}