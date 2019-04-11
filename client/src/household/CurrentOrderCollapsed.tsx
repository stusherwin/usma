import * as React from 'react';
import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { RouterLink } from '../common/RouterLink'

export interface CurrentOrderCollapsedProps { household: Household
                                            , currentOrder: HouseholdOrder | null
                                            , currentCollectiveOrder: CollectiveOrder | null
                                            , basePath: string
                                            }

export class CurrentOrderCollapsed extends React.Component<CurrentOrderCollapsedProps, {}> {
  render() {
    let currentOrder = this.props.currentOrder
    let currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <RouterLink path={`${this.props.basePath}order`} className="bg-order-dark p-2 block no-underline hover:no-underline text-black hover:text-black">
        { currentOrder
        ? (
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative">
            <h2>Current order</h2>
            <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(currentOrder.orderCreatedDate)}</span><span><Money amount={currentOrder.totalIncVat} /></span></h3>
            <h3 className="font-normal">{currentOrder.status}</h3>
          </div>
        )
        : (currentCollectiveOrder
        ? (
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
            <h2>Current order</h2>
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's an order currently in progress: <strong>{Util.formatDate(currentCollectiveOrder.createdDate)}</strong></p>
          </div>
        )
        : (
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mb-1">
            <h2>Current order</h2>
            <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
          </div>
        ))
        }
      </RouterLink>
    )
  }
}