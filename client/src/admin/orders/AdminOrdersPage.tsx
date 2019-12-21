import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder, PastHouseholdOrder } from '../../util/Types'
import { CollapsibleState } from '../../util/Collapsible'

import { AdminTopNav } from '../AdminTopNav'

import { CurrentCollectiveOrder } from './CurrentCollectiveOrder'
import { PastOrders } from './PastOrders'

export interface AdminOrdersPageProps { collectiveOrder: CollectiveOrder | null
                                      , pastOrders: PastCollectiveOrder[]
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
        <CurrentCollectiveOrder collapsibleKey="order"
                                collapsibleState={this.state.collapsibleState}
                                {...this.props} />
        <PastOrders collapsibleKey="past-orders"
                    collapsibleState={this.state.collapsibleState}
                    {...this.props} />
      </div>
    )
  }
}