import * as React from 'react';

import { Household } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Form, Field, Validate } from '../Validation'

export interface HouseholdsPageProps { households: Household[]
                                     , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , reload: () => Promise<void>
                                     , loading: boolean
                                     , error: ApiError | null
                                     }

export interface HouseholdsPageState { creating: boolean
                                     , editingId: number | null
                                     , form: Form
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { creating: false
                 , editingId: null
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
        .then(this.props.reload)
        .then(_ => this.setState({ creating: false
                                 , form: this.state.form.reset({name: ''})
                                 })
        )
    }
  }

  startEdit = (household: Household) => this.setState({ editingId: household.id
                                                      , form: this.state.form.reset({name: household.name})
                                                      })

  cancelEdit = () => this.setState({ editingId: null
                                   , form: this.state.form.reset({name: ''})
                                   })

  confirmEdit = () => {
    if(!this.state.editingId) return

    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.state.editingId, validated.fields.name.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editingId: null
                                 , form: this.state.form.reset({name: ''})
                                 })
        )
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
        <div hidden={!this.props.loading}>Loading...</div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-img-household bg-no-repeat bg-16 pl-16 min-h-16 bg-household-light">
          <div>
            <RouterLink path="/orders">Orders</RouterLink>
            <RouterLink path="/products">Products</RouterLink>
            <RouterLink path="/households">Households</RouterLink>
          </div>
          <h1>Households</h1>
          {!this.state.creating && 
            <Button action={this.startCreate}>New household</Button>
          }
        </div>
        {this.state.creating &&
          <div>
            <span>
              <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
              {this.state.form.fields.name.error}
            </span>
            <Button action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Button>
            <Button action={this.cancelCreate}>Cancel</Button>
          </div>
        }
        {!this.props.households.length
        ? <div>No households created yet</div>
        : (
          <div>
            { this.props.households.map(h => 
            this.state.editingId == h.id
            ? (
              <div>
                <span>
                  <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
                  {this.state.form.fields.name.error}
                </span>
                <Button action={this.confirmEdit} disabled={!this.state.form.valid()}>Save</Button>
                <Button action={this.cancelEdit}>Cancel</Button>
              </div>
            )
            : (
              <div key={h.id}>
                <RouterLink path={`/households/${h.id}`}>{h.name}</RouterLink>
                <Button action={() => this.startEdit(h)}>Edit</Button>
                <Button action={() => this.delete(h)}>Delete</Button>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}