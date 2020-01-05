import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder as Order, Household } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Router } from '../../util/Router'
import { Collapsible, CollapsibleState } from '../../util/Collapsible'

import { CollectiveOrderTabs } from './CollectiveOrderTabs'
import { HouseholdOrders } from './HouseholdOrders'
import { CollectiveOrderItems } from './CollectiveOrderItems'
import { CollectiveOrderProductCodes } from './CollectiveOrderProductCodes'
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { CollectiveOrderMessages } from './CollectiveOrderMessages'
import { CollectiveOrderTotal } from './CollectiveOrderTotal'
import { CollectiveOrderStatus } from './CollectiveOrderStatus'

export interface CollectiveOrderProps { collectiveOrder: Order | null
                                        households: Household[]
                                        collapsibleKey: string
                                        collapsibleState: CollapsibleState
                                        request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        reload: () => Promise<void>
                                      }

export interface CollectiveOrderState { addingHousehold: Household | null
                                        tab: 'households' | 'product-list' | 'product-codes'
                                      }

export class CollectiveOrder extends React.Component<CollectiveOrderProps, CollectiveOrderState> {
  constructor(props: CollectiveOrderProps) {
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
                         <CollectiveOrderStatus order={order} />
                         <CollectiveOrderTotal order={order} />
                       </h3>
                       <CollectiveOrderButtons order={order}
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
              <CollectiveOrderMessages order={order} />
              { this.state.tab == 'households'?
                <HouseholdOrders order={order}
                                        {...this.props} />
              : this.state.tab == 'product-list'?
                <CollectiveOrderItems order={order} />
              : <CollectiveOrderProductCodes order={order} />
              }
            </div>
          }
      </Collapsible>
    )
  }
}