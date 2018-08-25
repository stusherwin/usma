import * as React from 'react';

import { Household } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Form, Field, Validate } from './Validation'

export interface HouseholdsPageProps { households: Household[]
                                     , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , reload: () => void
                                     }

export interface HouseholdsPageState { creating: boolean
                                     , form: Form
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { creating: false
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]) })
                 }
  }

  startCreate = () => this.setState({ creating: true
                                    })

  cancelCreate = () => this.setState({ creating: false
                                     , form: this.state.form.reset({name: ''})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(_ => {
          this.setState({ creating: false
                        , form: this.state.form.reset({name: ''})
                        })
          this.props.reload()
        })
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: this.state.form.update(fieldName, event.target.value) })

  delete = (h: Household) => 
    this.props.request(ServerApi.command.archiveHousehold(h.id))
      .then(this.props.reload)

  render() {
    return (
      <div>
        <h1>Households</h1>
        {!this.state.creating && 
          <Link action={this.startCreate}>New household</Link>
        }
        {this.state.creating &&
          <div>
            <span>
              <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
              {this.state.form.fields.name.error}
            </span>
            <Link action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Link>
            <Link action={this.cancelCreate}>Cancel</Link>
          </div>
        }
        {!this.props.households.length ? <div>No households created yet</div> : (
          <div>
            { this.props.households.map(h => (
              <div key={h.id}>
                <span>{h.name}</span>
                <RouterLink path={`/households/${h.id}/orders`}>View</RouterLink>
                <Link action={() => this.delete(h)}>Delete</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}