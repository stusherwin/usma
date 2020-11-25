import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, OrderItem as Item, CollectiveOrder, Household, GroupSettings } from 'util/Types'
import { Util } from 'util/Util'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { OrderItem } from 'order/OrderItem'
import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'
import { OrderFooter } from 'order/OrderFooter'

export interface PastHouseholdOrdersProps {
  household: Household
  collectiveOrder: CollectiveOrder | undefined
  groupSettings: GroupSettings
  collapsibleKey: string
  collapsibleState: CollapsibleState
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export interface HouseholdPaymentsState {
  collapsibleState: CollapsibleState
}

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, HouseholdPaymentsState> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({ collapsibleState }))
    }
  }

  addToCurrentOrder = (item: Item) => {
    if (!this.props.collectiveOrder) return;

    this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.collectiveOrder.id, this.props.household.id, item.productCode, null))
      .then(this.props.reload)
  }

  addAllItemsToCurrentOrder = (ho: HouseholdOrder) => {
    if (!this.props.collectiveOrder) return;

    this.props.request(ServerApi.command.ensureAllItemsFromPastHouseholdOrder(this.props.collectiveOrder.id, this.props.household.id, ho.orderId))
      .then(this.props.reload)
  }

  render() {
    const pastOrders = this.props.household.pastHouseholdOrders
    const total = pastOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)

    return (
      <Collapsible
        collapsibleKey={this.props.collapsibleKey}
        collapsibleState={this.props.collapsibleState}
        {...this.props}
        header={
          <div className="p-2 pt-4 bg-past-order border-past-order-dark border-b border-t min-h-24">
            <svg className="w-16 h-16 absolute">
              <use xlinkHref="#icon-order" />
            </svg>
            <div className="flex justify-between items-baseline">
              <h2 className="leading-none ml-20">
                Past orders
                         </h2>
              {this.props.groupSettings.enablePayments &&
                <h3>
                  <Money className="text-right" amount={total} />
                </h3>
              }
            </div>
          </div>
        }>
        <div className="shadow-inner-top bg-order-dark">
          {!pastOrders.length
            ? <div className="px-2 py-4 text-black">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div>
            : <table className="border-collapse w-full">
              <tbody>
                {pastOrders.map((ho, i) => {
                  return (
                    <tr key={ho.orderId}>
                      <td>
                        <Collapsible collapsibleKey={ho.orderId}
                          collapsibleState={this.state.collapsibleState}
                          header={
                            <div className={classNames('p-2 pt-4 bg-order-lightest min-h-24', { "shadow-inner-top": i == 0 })}>
                              <svg className="w-16 h-16 absolute">
                                <use xlinkHref="#icon-order" />
                              </svg>
                              <div className="flex items-baseline justify-between ml-20">
                                <div>
                                  <h3 className={classNames("leading-none", { 'line-through': ho.isAbandoned })}>
                                    {Util.formatDate(ho.orderCreatedDate)}
                                  </h3>
                                  <h4 className="mt-4 mb-4">
                                    <OrderStatus order={ho} />
                                  </h4>
                                </div>
                                <h4 className="ml-2">
                                  <OrderTotal order={ho} />
                                </h4>
                              </div>
                            </div>
                          }
                          expandedHeader={!!ho.items.length && (!this.props.household.currentHouseholdOrder || this.props.household.currentHouseholdOrder.isOpen) &&
                            <div className="flex justify-start p-2 bg-order-lightest pt-0">
                              <button className="ml-auto" onClick={e => { e.stopPropagation(); e.preventDefault(); this.addAllItemsToCurrentOrder(ho) }}><Icon type="add" className="w-4 h-4 fill-current nudge-d-2 mr-2" />Add all items to current order</button>
                            </div>
                            || undefined}>
                          <div className="shadow-inner-top bg-white">
                            {!ho.items.length ?
                              <div className="px-2 py-4 text-black">
                                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
                              </div>
                              : <div>
                                <table className="border-collapse w-full">
                                  <tbody>
                                    {ho.items.map(item =>
                                      <OrderItem
                                        item={item}
                                        past={true}
                                        orderAbandoned={ho.isAbandoned}
                                        addToCurrentOrder={(!this.props.household.currentHouseholdOrder || this.props.household.currentHouseholdOrder.isOpen) && this.addToCurrentOrder || undefined}
                                        {...this.props} />
                                    )}
                                    <OrderFooter order={ho} />
                                  </tbody>
                                </table>
                              </div>
                            }
                          </div>
                        </Collapsible>
                      </td>
                    </tr>
                  )
                })}
                {this.props.groupSettings.enablePayments &&
                  <tr>
                    <td className="pt-4 pl-20 pr-2 pb-4 font-bold">
                      <div className="pl-2 flex justify-between">
                        <span>Total:</span>
                        <span className="font-bold text-right"><Money amount={total} /></span>
                      </div>
                    </td>
                  </tr>
                }
              </tbody>
            </table>
          }
        </div>
      </Collapsible>
    )
  }
}