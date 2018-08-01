import * as React from 'react';

import { OrderHousehold } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'

export interface OrderHouseholdPageProps { orderId: number
                                         , householdId: number
                                         , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                         , navigate: (location: string) => void
                                         }

export interface OrderHouseholdPageState { details: OrderHousehold | null
                                         , initialised: boolean
                                         }

export class OrderHouseholdPage extends React.Component<OrderHouseholdPageProps, OrderHouseholdPageState> {
  constructor(props: OrderHouseholdPageProps) {
    super(props)

    this.state = { details: null
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getOrderHouseholdDetails(this.props.orderId, this.props.householdId))
      .then(details => this.setState({ details
                                     , initialised: true
                                     }))
      .catch(_ => this.setState({ initialised: true }))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    if(!this.state.details) return <div>Order not found.</div>

    return (
      <div>
        <div>
          <Link action={() => this.props.navigate('/orders')}>Orders</Link> &gt;
          <Link action={() => this.props.navigate('/orders/' + this.props.orderId)}>{Util.formatDate(this.state.details.orderCreatedDate)}</Link> &gt;
        </div>
        <h1>{this.state.details.householdName}</h1>
        {!this.state.details.cancelled ? <Link action={() => {}}>Cancel</Link> : null}
        <Link action={() => {}}>Record payment</Link>
        <div>
          {this.state.details.items.map(i => <div>
            <span>{i.productName}</span>
            <span>x {i.quantity}</span>
            <Money amount={i.total} />
            <Link action={() => {}}>Edit</Link>
          </div>)}
          <div>
            <span>Total:</span>
            <span></span>
            <Money amount={this.state.details.total} />
          </div>
        </div>
      </div>
    )
  }
}