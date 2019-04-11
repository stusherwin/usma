import * as React from 'react';

import { PastHouseholdOrder } from '../Types'
import { RouterLink } from '../common/RouterLink'
import { Money } from '../common/Money'

export interface PastHouseholdOrdersCollapsedProps { householdOrders: PastHouseholdOrder[]
                                                   , basePath: string
                                                   }

export class PastHouseholdOrdersCollapsed extends React.Component<PastHouseholdOrdersCollapsedProps, {}> {
  render() {
    const total = this.props.householdOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)
    const itemCount = (ho: PastHouseholdOrder) => {
      const sum = ho.items.reduce((tot, ho) => tot + ho.itemQuantity, 0)
      return sum + (sum == 1 ? ' item' : ' items')
    }

    return (
      <RouterLink path={`${this.props.basePath}past-orders`} className="bg-grey-lighter p-2 block no-underline hover:no-underline text-grey-darkest hover:text-grey-darkest">
        <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
          <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past orders</h2>
          <h3 className="mt-0 flex justify-between"><span>Total orders:</span><span><Money amount={total} /></span></h3>
        </div>
      </RouterLink>
    )
  }
}