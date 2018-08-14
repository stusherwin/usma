import * as React from 'react';

import { Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Form, Field, Validate } from './Validation'

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
                 , form: Form.create({ name: Field.create('', (v: string) => v, [Validate.required('Name is required')]) })
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
                                     , form: this.state.form.reset()
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(() => this.props.request(ServerApi.query.households()))
        .then(households => this.setState({ households
                                          , creating: false
                                          , form: this.state.form.reset()
                                          }))
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: this.state.form.update(fieldName, event.target.value) })

  delete = (h: Household) => 
    this.props.request(ServerApi.command.archiveHousehold(h.id))
      .then(() => this.props.request(ServerApi.query.households()))
      .then(households => this.setState({ households
                                        }))

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    return (
      <div>
        <h1>Households</h1>
        <Link action={this.startCreate}>New household</Link>
        {this.state.creating &&
          <div>
            <span>
              <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
              {this.state.form.fields.name.error}
            </span>
            <Link action={this.confirmCreate} disabled={!this.state.form.valid()}>Add</Link>
            <Link action={this.cancelCreate}>Cancel</Link>
          </div>
        }
        {!this.state.households.length ? <div>No households created yet</div> : (
          <div>
            { this.state.households.map(h => (
              <div key={h.id}>
                <span>{h.name}</span>
                <Link action={() => this.delete(h)}>Delete</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}