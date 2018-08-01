import * as React from 'react';

import { Order } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'

export interface OrderPageProps { id: number
                                , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                , navigate: (location: string) => void
                                }

export interface OrderPageState { order: Order | null
                                , initialised: boolean
                                }

export class OrderPage extends React.Component<OrderPageProps, OrderPageState> {
  constructor(props: OrderPageProps) {
    super(props)

    this.state = { order: null
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getOrder(this.props.id))
      .then(order => this.setState({ order
                                   , initialised: true
                                   }))
      .catch(_ => this.setState({ initialised: true }))
  }

  delete = () => {
    this.props.request(ServerApi.deleteOrder(this.props.id)).then(_ => this.props.navigate(''))
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    if(!this.state.order) return <div>Order not found.</div>

    return (
      <div>
        <h1>Order: {Util.formatDate(this.state.order.createdDate)}</h1>
        <Link action={this.delete}>Delete</Link>
      </div>
    )
  }
}