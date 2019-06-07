import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household, PastCollectiveOrder, PastHouseholdOrder } from '../../Types'
import { ApiError } from '../../ServerApi'
import { TopNav } from '../TopNav'
import { CurrentOrder } from './CurrentOrder'
import { PastOrders } from './PastOrders'

export interface OrdersPageProps { currentOrder: CollectiveOrder | null
                                 , currentHouseholdOrders: HouseholdOrder[]
                                 , pastOrders: PastCollectiveOrder[]
                                 , pastHouseholdOrders: PastHouseholdOrder[]
                                 , households: Household[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , reload: () => Promise<void>
                                 , loading: boolean
                                 , error: ApiError | null
                                 }
type Section = 'order' | 'past-orders'
export interface OrdersPageState { expanded: Section | null }

export class OrdersPage extends React.Component<OrdersPageProps, OrdersPageState> {  
  constructor(props: OrdersPageProps) {
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
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <TopNav />
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