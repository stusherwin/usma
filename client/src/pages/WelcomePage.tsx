import * as React from 'react';

import { Household } from '../Types'
import { RouterLink } from '../RouterLink'
import { Icon } from '../Icon'
import { Router } from '../Router'
import { Form, Field, Validate } from '../Validation'
import { TextField } from '../Field'
import { ServerApi, ApiError } from '../ServerApi'

export interface WelcomePageProps { households: Household[]
                                  , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                  , reload: () => Promise<void>
                                  , loading: boolean
                                  , error: ApiError | null
                                  }

export interface WelcomePageState { selectedHouseholdId: number | undefined
                                  , editing: 'new' | number | null
                                  , form: Form
                                  }

export class WelcomePage extends React.Component<WelcomePageProps, WelcomePageState> {
  constructor(props: WelcomePageProps) {
    super(props)

    this.state = { selectedHouseholdId: props.households.length ? props.households[0].id : undefined
                 , editing: null
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]) })
                 }
  }

  selectedHouseholdChanged = (event: any) => {
    this.setState({ selectedHouseholdId: parseInt(event.target.value) })
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })

  startCreate = () => this.setState({ editing: 'new'
                                    , form: this.state.form.reset({name: ''})
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({name: ''})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value))
        .then(id => this.props.reload()
          .then(_ => Router.navigate('/admin/households/' + id)))
    }
  }

  continue = () => {
    Router.navigate('/admin/households/' + this.state.selectedHouseholdId)
  }

  render() {
    return (
      <div className="h-screen">
        <div className="collective-orders-header"><h1>Collective Orders</h1></div>
        <div className="h-screen bg-household-light text-household-darker">
            <h2 className="leading-none py-4 px-2">Welcome</h2>
            {!!this.props.households.length && !this.state.editing &&
              <div className="p-2">
                <p className="mb-4">Choose your household to continue:</p>
                <select value={this.state.selectedHouseholdId} onChange={this.selectedHouseholdChanged}>
                  {this.props.households.map(h => <option key={h.id} value={h.id}>{h.name}</option>)}
                </select>
                <div className="mt-4">
                  <button className="" onClick={this.continue}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Continue</button>
                </div>
                <p className="mt-4">Not in the list?</p>
                <div className="mt-4">
                  <button className="" onClick={this.startCreate}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add your household</button>
                </div>
              </div>
            }
            {(!this.props.households.length || this.state.editing == 'new') &&
              <div className="bg-household-lightest p-2">
                <h3 className="mb-4">Enter a name for your household:</h3>
                <TextField id="create-name"
                           label="Name"
                           field={this.state.form.fields.name}
                           valueOnChange={this.fieldChanged('name')} />
                <div className="flex justify-end">
                  <button className="ml-2" onClick={this.confirmCreate} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
                  <button className="ml-2" onClick={this.cancelCreate}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
                </div>
              </div>
            }
        </div>
      </div>
    )
  }
}