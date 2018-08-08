import * as React from 'react';

import { FullOrderSummary, Product, OrderSummary_Item } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface FullOrderPageProps { orderId: number
                                    , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                    , navigate: (location: string) => void
                                    }

export interface FullOrderPageState { summary: FullOrderSummary | null
                                    , initialised: boolean
                                    }

export class FullOrderPage extends React.Component<FullOrderPageProps, FullOrderPageState> {
  constructor(props: FullOrderPageProps) {
    super(props)

    this.state = { summary: null
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.query.fullOrderSummary(this.props.orderId))
      .then(summary => this.setState({ summary
                                     , initialised: true
                                     }))
      .catch(_ => this.setState({ initialised: true }))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    const summary = this.state.summary
    if(!summary) return <div>Order not found.</div>

    return (
      <div>
        <div>
          <Link action={_ => this.props.navigate('/orders')}>Orders</Link> &gt;
          <Link action={_ => this.props.navigate('/orders/' + this.props.orderId)}>{Util.formatDate(summary.orderCreatedDate)}</Link> &gt;
        </div>
        <h1>Full order list</h1>
        <div>
          {summary.items.map(i => (
            <div>
              <span>{i.productName}</span>
              <span>x {i.quantity}</span>
              <Money amount={i.total} />
            </div>
          ))}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={summary.total} />
          </div>
        </div>
      </div>
    )
  }
}