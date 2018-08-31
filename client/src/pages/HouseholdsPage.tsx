import * as React from 'react';
import * as classNames from 'classnames'

import { Household } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { RouterLink } from '../RouterLink'
import { Button } from '../Button'
import { Form, Field, Validate } from '../Validation'
import { TopNav } from '../TopNav'

export interface HouseholdsPageProps { households: Household[]
                                     , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                     , reload: () => Promise<void>
                                     , loading: boolean
                                     , error: ApiError | null
                                     }

export interface HouseholdsPageState { editMode: 'create' | 'edit' | 'view'
                                     , editingId: number | null
                                     , form: Form
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { editMode: 'view'
                 , editingId: null
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]) })
                 }
  }

  startCreate = () => this.setState({ editMode: 'create'
                                    , form: this.state.form.reset({name: ''})
                                    })

  cancelCreate = () => this.setState({ editMode: 'view'
                                     , form: this.state.form.reset({name: ''})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editMode: 'view'
                                 , form: this.state.form.reset({name: ''})
                                 })
        )
    }
  }

  startEdit = (household: Household) => this.setState({ editMode: 'edit'
                                                      , editingId: household.id
                                                      , form: this.state.form.reset({name: household.name})
                                                      })

  cancelEdit = () => this.setState({ editMode: 'view'
                                   , editingId: null
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
        .then(_ => this.setState({ editMode: 'view'
                                 , editingId: null
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
        <div className="bg-household-light p-2">
          <TopNav />
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative">
            <h1 className="leading-none mb-2">Households</h1>
          </div>
        </div>
        <div>
          {this.state.editMode != 'create' && 
            <div className="flex justify-end px-2 my-2">
              <Button action={this.startCreate}>New household</Button>
            </div>
          }
          {this.state.editMode == 'create' &&
            <div className="bg-purple-lightest p-2 mb-2">
              <div className="flex justify-between">
                <label className="flex-no-grow flex-no-shrink mr-2"
                       htmlFor="create-name">Name</label>
                <input type="text"
                       id="create-name" 
                       className={classNames('flex-grow flex-no-shrink', {'invalid': !this.state.form.fields.name.valid})}
                       autoFocus 
                       value={this.state.form.fields.name.stringValue} 
                       onChange={this.fieldChanged('name')} />
              </div>
              <div hidden={!this.state.form.fields.name.error}>
                {this.state.form.fields.name.error}
              </div>
              <div className="mt-2 flex justify-end">
                <Button className="ml-2" action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Button>
                <Button className="ml-2" action={this.cancelCreate}>Cancel</Button>
              </div>
            </div>
          }
          {!this.props.households.length
          ? <div>No households created yet</div>
          : (
            <div>
              { this.props.households.map(h => 
              this.state.editMode == 'edit' && this.state.editingId == h.id
              ? (
                <div className="bg-purple-lightest p-2 mb-2">
                  <div className="flex justify-between">
                    <label className="flex-no-grow flex-no-shrink mr-2"
                           htmlFor="edit-name">Name</label>
                    <input type="text"
                           id="edit-name" 
                           className={classNames('flex-grow flex-no-shrink', {'invalid': !this.state.form.fields.name.valid})} 
                           autoFocus 
                           value={this.state.form.fields.name.stringValue} 
                           onChange={this.fieldChanged('name')} />
                    {this.state.form.fields.name.error}
                  </div>
                  <div hidden={!this.state.form.fields.name.error}>
                    {this.state.form.fields.name.error}
                  </div>
                  <div className="mt-2 flex justify-end">
                    <Button icon="ok" className="ml-2" action={this.confirmEdit} disabled={!this.state.form.valid()}></Button>
                    <Button icon="cancel-circle" className="ml-2" action={this.cancelEdit}></Button>
                  </div>
                </div>
              )
              : (
                <div key={h.id} className="flex justify-between items-baseline px-2 mb-2">
                  <RouterLink className="flex-grow" path={`/households/${h.id}`}>{h.name}</RouterLink>
                  <span className="flex-no-shrink flex-no-grow">
                    <Button icon="edit" action={() => this.startEdit(h)}></Button>
                    <Button icon="trash" className="ml-2" action={() => this.delete(h)}></Button>
                  </span>
                </div>
              )) }
            </div>
          )}
        </div>
      </div>
    )
  }
}