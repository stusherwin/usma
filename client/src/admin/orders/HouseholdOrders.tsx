import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder } from 'util/Types'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'

import { HouseholdOrderItems } from 'household/HouseholdOrderItems'

import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'

export interface HouseholdOrdersProps { order: CollectiveOrder
                                        request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        reload: () => Promise<void>
                                      }

export interface HouseholdOrdersState { collapsibleState: CollapsibleState }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, HouseholdOrdersState> {
  constructor(props: HouseholdOrdersProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
    }
  }

  render() {
    const order = this.props.order

    return !order.householdOrders.filter(ho => !!ho.items.length).length?
      <div className="px-2 py-4 text-black">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order
      </div>
    : <div className="mt-4">
        {order.householdOrders.filter(ho => !!ho.items.length).map(ho => {
          return (
            <div key={ho.householdId}>
              <Collapsible className="min-h-24"
                           collapsibleKey={ho.householdId}
                           collapsibleState={this.state.collapsibleState}
                           header={
                             <div className="p-2 pt-4 bg-household-lighter h-24">
                               <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                               <div className="flex justify-between">
                                 <h3 className={classNames("leading-none ml-20", {"line-through": ho.isAbandoned})}>
                                   {ho.householdName}
                                 </h3>
                                 <h4>
                                   <OrderTotal order={ho} />
                                 </h4>
                               </div>
                               <h4 className="ml-20 mt-4 mb-4">
                                 <OrderStatus order={ho} />
                               </h4>
                             </div>
                           }>
                <div className="shadow-inner-top bg-white border-t border-household-light">
                  <HouseholdOrderItems householdOrder={ho}
                                       readOnly={true}
                                       {...this.props} />
                </div>
              </Collapsible>
            </div>
          )}
        )}
        <div className="pt-4 pb-4 pl-20 pr-2 font-bold text-black pl-2 flex justify-between">
          <span className="pl-2">Total:</span>
          {/* <span className={classNames('text-right', {'line-through text-grey-dark': order.isAbandoned})}><Money amount={order.totalIncVat} /></span> */}
          <span className="font-bold text-right">
            { order.adjustment == null || order.adjustment.oldTotalIncVat == order.totalIncVat?
              <Money className={classNames({'line-through text-black': order.isAbandoned})} amount={order.totalIncVat} />
            : <span>
                <Money className="line-through text-black mr-2" amount={order.adjustment.oldTotalIncVat} />
                <Money className="text-red" amount={order.totalIncVat} />
              </span>
            }
          </span>
        </div>
    </div>
  }
}