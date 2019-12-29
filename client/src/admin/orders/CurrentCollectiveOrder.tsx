import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, Household } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Router } from '../../util/Router'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { CollectiveOrderTabs } from './CollectiveOrderTabs'
import { CurrentHouseholdOrders } from './CurrentHouseholdOrders'
import { CurrentCollectiveOrderItems } from './CurrentCollectiveOrderItems'
import { CurrentCollectiveOrderProductCodes } from './CurrentCollectiveOrderProductCodes'
import { CurrentCollectiveOrderButtons } from './CurrentCollectiveOrderButtons'
import { CurrentCollectiveOrderMessages } from './CurrentCollectiveOrderMessages'
import { CurrentCollectiveOrderTotal } from './CurrentCollectiveOrderTotal'
import { CurrentCollectiveOrderStatus } from './CurrentCollectiveOrderStatus'

export interface CurrentCollectiveOrderProps { collectiveOrder: CollectiveOrder | null
                                             , households: Household[]
                                             , collapsibleKey: string
                                             , collapsibleState: CollapsibleState
                                             , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                             , reload: () => Promise<void>
                                             }

export interface CurrentCollectiveOrderState { addingHousehold: Household | null
                                   , tab: 'households' | 'product-list' | 'product-codes'
                                   }

export class CurrentCollectiveOrder extends React.Component<CurrentCollectiveOrderProps, CurrentCollectiveOrderState> {
  constructor(props: CurrentCollectiveOrderProps) {
    super(props)

    this.state = { addingHousehold: null
                 , tab: 'households'
                 }
  }

  newOrder = () => {
    // TODO: Have some way of choosing the household to create order
    this.props.request(ServerApi.command.createOrder(this.props.households[0].id))
      .then(this.props.reload)
  }

  deleteOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.deleteOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/admin/orders`))
  }

  abandonOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  placeOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.placeOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  render() {
    const order = this.props.collectiveOrder

    return (
      <Collapsible className="min-h-20"
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={
                     <div className="p-2 bg-order-dark min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Current order
                       </h2>
                       <h3 className="flex justify-between ml-20 mt-4">
                         <CurrentCollectiveOrderStatus order={order} />
                         <CurrentCollectiveOrderTotal order={order} />
                       </h3>
                       <CurrentCollectiveOrderButtons order={order}
                          newOrder={this.newOrder} deleteOrder={this.deleteOrder} abandonOrder={this.abandonOrder} placeOrder={this.placeOrder} />
                       {!!order
                       ? <CollectiveOrderTabs tab={this.state.tab} setTab={tab => this.setState({tab})} />
                       : <span></span>
                       }
                     </div>
                   }>
          { order && 
            <div className={classNames("shadow-inner-top border-t", {
              "bg-household-lightest": this.state.tab == 'households',
              "bg-white": this.state.tab != 'households'
            })}>
              <CurrentCollectiveOrderMessages order={order} />
              { this.state.tab == 'households'?
                <CurrentHouseholdOrders order={order}
                                        {...this.props} />
              : this.state.tab == 'product-list'?
                <CurrentCollectiveOrderItems collectiveOrder={order} />
              : <CurrentCollectiveOrderProductCodes collectiveOrder={order} />
              }
            </div>
          }
      </Collapsible>
    )
  }
}