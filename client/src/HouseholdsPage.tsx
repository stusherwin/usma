import * as React from 'react';

import { Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'

export interface HouseholdsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , navigate: (location: string) => void
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, { households: Household[], initialised: boolean }> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { households: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.getHouseholds())
      .then(households => {
        this.setState({ households
                      , initialised: true
                      })
      })
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>

    return (
      <div>
        <h1>Households</h1>
      </div>
    )
  }
}