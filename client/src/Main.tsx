import * as React from 'react';

import { Order } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

export interface MainProps {}
export interface MainState { initialDataLoaded: boolean
                           , orders: Order[]
                           , error: ApiError | null
                           }

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    this.state = { initialDataLoaded: false
                 , orders: []
                 , error: null
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.getOrders()])
      .then(results => {
        let orders = results[0]

        this.setState({ orders
                      , initialDataLoaded: true
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ error: apiError
                      })
      })
  }

  render() {
    return (
      <div>
      <h1>Orders</h1>
        <ul>
          { this.state.orders.map(o => <li>{ Util.toDateString(o.date) }</li>) }
        </ul>
      </div>
    )
  }
}