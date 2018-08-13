import * as React from 'react';

import { Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'

export interface HouseholdsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , navigate: (location: string) => void
                                     }

export interface HouseholdsPageState { households: Household[]
                                     , initialised: boolean
                                     , newHouseholdName: string | null
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { households: []
                 , initialised: false
                 , newHouseholdName: null
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.query.households())
      .then(households => {
        this.setState({ households
                      , initialised: true
                      })
      })
  }

  startCreate = () => this.setState({ newHouseholdName: '' })

  cancelCreate = () =>
    this.setState({ newHouseholdName: null })

  confirmCreate = () => {
    if(this.state.newHouseholdName == null) return

    this.props.request(ServerApi.command.createHousehold(this.state.newHouseholdName))
      .then(() => this.props.request(ServerApi.query.households()))
      .then(households => this.setState({ households
                                        , newHouseholdName: null
                                        }))
  }

  newHouseholdNameChanged = (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ newHouseholdName: event.target.value })

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    return (
      <div>
        <h1>Households</h1>
        <Link action={this.startCreate}>New household</Link>
          {this.state.newHouseholdName != null &&
            <div>
              <input type="text" value={this.state.newHouseholdName} onChange={this.newHouseholdNameChanged} />
              <Link action={this.confirmCreate}>Add</Link>
              <Link action={this.cancelCreate}>Cancel</Link>
            </div>
          }
        {!this.state.households.length ? <div>No households created yet</div> : (
          <div>
            { this.state.households.map(h => (
              <div key={h.id}>
                <span>{h.name}</span>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}