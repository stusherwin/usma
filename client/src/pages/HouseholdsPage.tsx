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

export interface HouseholdsPageState { editing: 'new' | number | null
                                     , form: Form
                                     }

export class HouseholdsPage extends React.Component<HouseholdsPageProps, HouseholdsPageState> {
  constructor(props: HouseholdsPageProps) {
    super(props)

    this.state = { editing: null
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]) })
                 }
  }

  startCreate = () => this.setState({ editing: 'new'
                                    , form: this.state.form.reset({name: ''})
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({name: ''})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: ''})
                                 })
        )
    }
  }

  startEdit = (household: Household) => this.setState({ editing: household.id
                                                      , form: this.state.form.reset({name: household.name})
                                                      })

  cancelEdit = () => this.setState({ editing: null
                                   , form: this.state.form.reset({name: ''})
                                   })

  confirmEdit = () => {
    if(typeof this.state.editing !== 'number') return

    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.state.editing, validated.fields.name.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
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
          <TopNav className="text-household-dark hover:text-household-darker" />
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4 overflow-auto">
            <h1 className="leading-none mb-2 -mt-1 text-household-darker">Households</h1>
            <div className="flex justify-start">
              <Button action={this.startCreate} disabled={!!this.state.editing}>New household</Button>
            </div>
          </div>
        </div>
        <div>
          {this.state.editing == 'new' &&
            <div className="bg-purple-lightest p-2 pt-4 border-t border-b">
              <div className={classNames('field', {'invalid': !this.state.form.fields.name.valid})}>
                <div className="flex justify-between items-baseline">
                  <label className="flex-no-grow flex-no-shrink mr-2"
                         htmlFor="create-name">Name</label>
                  <input type="text"
                         id="create-name" 
                         className="flex-grow flex-no-shrink"
                         autoFocus 
                         value={this.state.form.fields.name.stringValue} 
                         onChange={this.fieldChanged('name')} />
                </div>
                <div className="text-red mt-2" hidden={!this.state.form.fields.name.error}>
                  {this.state.form.fields.name.error}
                </div>
              </div>
              <div className="mt-4 flex justify-end">
                <Button className="ml-2" action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Button>
                <Button className="ml-2" action={this.cancelCreate}>Cancel</Button>
              </div>
            </div>
          }
          {!this.props.households.length
          ? <div>No households created yet</div>
          : (
            <div>
              { this.props.households.map((h, i) => 
              this.state.editing == h.id
              ? (
                <div className={classNames('bg-purple-lightest p-2 pt-4 border-t border-b', {'mt-2': i > 0})}>
                  <div className={classNames('field', {'invalid': !this.state.form.fields.name.valid})}>
                    <div className="flex justify-between items-baseline">
                      <label className="flex-no-grow flex-no-shrink mr-2"
                             htmlFor="edit-name">Name</label>
                      <input type="text"
                             id="edit-name" 
                             className="flex-grow flex-no-shrink"
                             autoFocus 
                             value={this.state.form.fields.name.stringValue} 
                             onChange={this.fieldChanged('name')} />
                    </div>
                    <div className="text-red mt-2" hidden={!this.state.form.fields.name.error}>
                      {this.state.form.fields.name.error}
                    </div>
                  </div>
                  <div className="mt-4 flex justify-end">
                    <Button className="ml-2" action={this.confirmEdit} disabled={!this.state.form.valid()}>Save</Button>
                    <Button className="ml-2" action={this.cancelEdit}>Cancel</Button>
                  </div>
                </div>
              )
              : (
                <div key={h.id} className="flex justify-between items-baseline px-2 mt-2">
                  <RouterLink className="flex-grow" path={`/households/${h.id}`}>{h.name}</RouterLink>
                  <span className="flex-no-shrink flex-no-grow">
                    <Button icon="edit" className="ml-2" action={() => this.startEdit(h)} disabled={!!this.state.editing}></Button>
                    <Button icon="delete" className="ml-2" action={() => this.delete(h)} disabled={!!this.state.editing}></Button>
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