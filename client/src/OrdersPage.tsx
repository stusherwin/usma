import * as React from 'react';

import { CollectiveOrder, HouseholdOrder, Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'
import { CurrentOrder } from './CurrentOrder'

export interface OrdersPageProps { currentOrder: CollectiveOrder | undefined
                                 , currentHouseholdOrders: HouseholdOrder[]
                                 , pastOrders: CollectiveOrder[]
                                 , households: Household[]
                                 , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                 , reload: () => Promise<void>
                                 }

export class OrdersPage extends React.Component<OrdersPageProps, {}> {
  newOrder = () => {
    this.props.request(ServerApi.command.createOrder())
      .then(this.props.reload)
  }

  render() {
    const currentOrder = this.props.currentOrder
    const pastOrders = this.props.pastOrders

    return (
      <div>
        <div className="bg-img-order bg-no-repeat bg-16 pl-16 min-h-16">
          <h1>Orders</h1>
        </div>
        <div>
          {currentOrder
          ? (
            <div>
              <h2>Current order: {Util.formatDate(currentOrder.createdDate)}</h2>
              <CurrentOrder order={currentOrder}
                            householdOrders={this.props.currentHouseholdOrders}
                            households={this.props.households}
                            reload={this.props.reload}
                            request={this.props.request} />
            </div>
          )
          : ( 
            <div>
              <h2>Current order</h2>
              <p>There's no order currently in progress.</p>
              <Button action={this.newOrder}>Start a new one</Button>
            </div>
          )}
        </div>
        <h2>Past orders</h2>
        {!pastOrders.length ? <div>No past orders</div> : (
          <div>
            { pastOrders.map(o => (
              <div key={o.id}>
                <RouterLink path={`/orders/${o.id}`}>{ Util.formatDate(o.createdDate)}</RouterLink>
                <span>{o.status}</span>
                <Money amount={o.total} />
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}