import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder, PastHouseholdOrder } from '../Types'
import { ApiError } from '../ServerApi'
import { AdminTopNav } from '../components/AdminTopNav'
import { CurrentOrder } from '../components/CurrentOrder'
import { PastOrders } from '../components/PastOrders'

export interface AdminOrdersPageProps { currentOrder: CollectiveOrder | null
                                      , currentHouseholdOrders: HouseholdOrder[]
                                      , pastOrders: PastCollectiveOrder[]
                                      , pastHouseholdOrders: PastHouseholdOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }
type Section = 'order' | 'past-orders'
export interface AdminOrdersPageState { expanded: Section | null }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'order', 
    }
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <CurrentOrder expanded={this.state.expanded == 'order'}
                      otherExpanding={!!this.state.expanded && this.state.expanded != 'order'}
                      toggle={this.toggle('order')}
                      {...this.props} />
        <PastOrders expanded={this.state.expanded == 'past-orders'}
                    otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders'}
                    toggle={this.toggle('past-orders')}
                    {...this.props} />
      </div>
    )
  }
}