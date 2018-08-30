import * as React from 'react';

import { CollectiveOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Money } from '../Money'
import { TopNav } from '../TopNav'

export interface FullOrderPageProps { order: CollectiveOrder
                                    , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                    , loading: boolean
                                    , error: ApiError | null
                                    }

export class FullOrderPage extends React.Component<FullOrderPageProps, {}> {
  constructor(props: FullOrderPageProps) {
    super(props)

    this.state = { summary: null
                 , initialised: false
                 }
  }

  render() {
    return (
      <div>
        <div hidden={!this.props.loading}>Loading...</div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-img-order bg-no-repeat bg-16 pl-16 min-h-16">
          <TopNav />
          <div>
            <RouterLink path="/orders">Orders</RouterLink> &gt;
            <RouterLink path={`/orders/${this.props.order.id}`}>{Util.formatDate(this.props.order.createdDate)}</RouterLink> &gt;
          </div>
          <h1>Full order list</h1>
        </div>
        <div>
          {this.props.order.items.map(i => (
            <div key={i.productId}>
              <span>{i.productName}</span>
              <span>x {i.itemQuantity}</span>
              <Money amount={i.itemTotal} />
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={this.props.order.total} />
          </div>
        </div>
      </div>
    )
  }
}