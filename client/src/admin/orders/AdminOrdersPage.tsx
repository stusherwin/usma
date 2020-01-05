import * as React from 'react';

import { CollectiveOrder as Order, Household, PastCollectiveOrder } from '../../util/Types'
import { CollapsibleState } from '../../util/Collapsible'

import { AdminTopNav } from '../AdminTopNav'

import { CollectiveOrder } from './CollectiveOrder'
import { PastCollectiveOrders } from './PastCollectiveOrders'

export interface AdminOrdersPageProps { collectiveOrder: Order | null
                                      , pastOrders: PastCollectiveOrder[]
                                      , households: Household[]
                                      , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                      , reload: () => Promise<void>
                                      }

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
        <CollectiveOrder collapsibleKey="order"
                                collapsibleState={this.state.collapsibleState}
                                {...this.props} />
        <PastCollectiveOrders collapsibleKey="past-orders"
                    collapsibleState={this.state.collapsibleState}
                    {...this.props} />
      </div>
    )
  }
}