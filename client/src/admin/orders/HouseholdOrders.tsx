import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, ProductCatalogueEntry } from 'util/Types'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { CollapsibleState } from 'util/Collapsible'

import { HouseholdOrder } from './HouseholdOrder'

export interface HouseholdOrdersProps {
  order: CollectiveOrder
  products: ProductCatalogueEntry[]
  categories: string[]
  brands: string[]
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export interface HouseholdOrdersState {
  collapsibleState: CollapsibleState
}

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, HouseholdOrdersState> {
  constructor(props: HouseholdOrdersProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({ collapsibleState })),
    }
  }

  render() {
    const order = this.props.order

    return !order.householdOrders.filter(ho => !!ho.items.length).length ?
      <div className="px-2 py-4 text-black">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order
      </div>
      : <div className="mt-4">
        {order.householdOrders.filter(ho => !!ho.items.length).map(ho => 
          <HouseholdOrder order={order}
                          householdOrder={ho}
                          collapsibleState={this.state.collapsibleState}
                          {...this.props}
          />
        )}
        <div className="pt-4 pb-4 pl-20 pr-2 font-bold text-black pl-2 flex justify-between">
          <span className="pl-2">Total:</span>
          <span className="font-bold text-right">
            {order.adjustment == null || order.adjustment.oldTotalIncVat == order.totalIncVat ?
              <Money className={classNames({ 'line-through text-black': order.isAbandoned })} amount={order.totalIncVat} />
              : <span className="inline-flex flex-col">
                <Money className="line-through text-black" amount={order.adjustment.oldTotalIncVat} />
                <Money className="text-red" amount={order.totalIncVat} />
              </span>
            }
          </span>
        </div>
      </div>
  }
}