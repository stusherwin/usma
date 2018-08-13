import * as React from 'react';

import { Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Validator, Form } from './Validator'

export interface HouseholdsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , navigate: (location: string) => void
                                     }

export interface HouseholdsPageState { households: Household[]
                                     , initialised: boolean
                                     , creating: boolean
                                     , form: Form
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { households: []
                 , initialised: false
                 , creating: false
                 , form: Validator.form({ name: Validator.field('', (v: string) => v, [{ validate: (v: string) => !!v.length, error: 'Name is required' }]) })
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

  startCreate = () => this.setState({ creating: true
                                    })

  cancelCreate = () => this.setState({ creating: false
                                     , form: Validator.reset(this.state.form)
                                     })

  confirmCreate = () => {
    const validated = Validator.validate(this.state.form)
    this.setState({ form: validated })
    if(validated.valid) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(() => this.props.request(ServerApi.query.households()))
        .then(households => this.setState({ households
                                          , creating: false
                                          , form: Validator.reset(this.state.form)
                                          }))
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: Validator.update(this.state.form, fieldName, event.target.value) })

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    return (
      <div>
        <h1>Households</h1>
        <Link action={this.startCreate}>New household</Link>
        {this.state.creating &&
          <div>
            <input type="text" value={this.state.form.fields.name.stringValue} className={this.state.form.fields.name.valid? 'valid': 'invalid'} onChange={this.fieldChanged('name')} />
            <Link action={this.confirmCreate} disabled={!this.state.form.valid}>Add</Link>
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