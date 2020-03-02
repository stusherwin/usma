import * as React from 'react';
import * as classNames from 'classnames'

import { Household } from 'util/Types'
import { RouterLink } from 'util/RouterLink'
import { Icon } from 'util/Icon'
import { Router } from 'util/Router'
import { Money } from 'util/Money'
import { Form, Field, Validate } from 'util/Validation'
import { TextField } from 'util/Field'
import { ServerApi } from 'util/ServerApi'

export interface HouseholdWelcomePageProps { households: Household[]
                                           , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                           , reload: () => Promise<void>
                                           }

export interface HouseholdWelcomePageState { selectedHouseholdId: number | undefined
                                           , editing: 'new' | number | null
                                           , form: Form
                                           }

export class HouseholdWelcomePage extends React.Component<HouseholdWelcomePageProps, HouseholdWelcomePageState> {
  constructor(props: HouseholdWelcomePageProps) {
    super(props)

    this.state = { selectedHouseholdId: !!props.households.length ? props.households[0].id : undefined
                 , editing: null
                 , form: Form.create({ 
                     name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]),
                     contactName: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
                     contactEmail: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
                     contactPhone: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', [])
                   })
                 }
  }

  selectedHouseholdChanged = (event: any) => {
    this.setState({ selectedHouseholdId: parseInt(event.target.value) })
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })

  startCreate = () => this.setState({ editing: 'new'
                                    , form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.createHousehold(validated.fields.name.value, validated.fields.contactName.value, validated.fields.contactEmail.value, validated.fields.contactPhone.value))
        .then(id => this.props.reload()
          .then(_ => Router.navigate('/households/' + id)))
    }
  }

  continue = () => {
    Router.navigate('/households/' + this.state.selectedHouseholdId)
  }

  render() {
    return (
      <div className="h-screen bg-household-light text-household-darker">
        {/* <div className="collective-orders-header">
          <h1>Collective Orders</h1>
        </div> */}
        <div className="p-2 block no-underline text-black hover:text-black hover:no-underline min-h-24">
          <div className="bg-no-repeat w-16 h-16 absolute bg-img-household mt-2"></div>
          <h2 className="leading-none ml-20 relative flex mt-2">Welcome
          </h2>
          {!this.props.households.length &&
            <div>
              <p className="mt-2 ml-20">You're the first! Please enter some details about your household</p>
            </div>
          }
          {!!this.props.households.length &&
            <div>
              <p className="mt-4 ml-20">Choose your household to continue.</p>
            </div>
          }
        </div>
        {!!this.props.households.length && !this.state.editing &&
          <div className="bg-household-lighter">
            {this.props.households.map((h, i) => 
               <RouterLink className="block no-underline text-black hover:underline hover:text-black relative" path={`/households/${h.id}`}>
                 <div className={classNames("p-2 pt-4 bg-household-lighter h-24", {"shadow-inner-top": i == 0})}>
                   <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                   <div className="flex justify-between">
                     <h3 className="leading-none ml-20 pr-20">
                       {h.name} 
                       <span className="text-black"><Icon type="right-arrow" className="w-3 h-3 fill-current ml-1 nudge-d-1" /></span>
                     </h3>
                     <h4 className="absolute pin-r mr-2 -mt-1 py-1 border-t-2 border-b-2 border-black">
                       <Money amount={-h.balance} noColour />
                     </h4>
                   </div>
                   <div className="mt-4 ml-20 absolute text-black flex justify-between">
                     <div className="text-lg"><strong>Contact:</strong> {h.contactName || 'none'}</div>
                   </div>
                 </div>
               </RouterLink>
            )}
            <div className="flex p-2 pb-4">
              <p className="mt-2 mr-4">Not in the list?</p>
              <button className="" onClick={this.startCreate}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add your household</button>
            </div>
          </div>
        }
        {(!this.props.households.length || this.state.editing == 'new') &&
          <div className="shadow-inner-top bg-household-lightest p-2 py-4">
            <h3 className="mb-4">Your household details</h3>
            <TextField id="create-name"
                       label="Name"
                       field={this.state.form.fields.name}
                       autofocus
                       valueOnChange={this.fieldChanged('name')} />
            <TextField id="create-contactName"
                       label="Contact name"
                       field={this.state.form.fields.contactName}
                       valueOnChange={this.fieldChanged('contactName')} />
            <TextField id="create-contactEmail"
                       label="Contact email"
                       field={this.state.form.fields.contactEmail}
                       valueOnChange={this.fieldChanged('contactEmail')} />
            <TextField id="create-contactPhone"
                       label="Contact phone"
                       field={this.state.form.fields.contactPhone}
                       valueOnChange={this.fieldChanged('contactPhone')} />
            <div className="flex justify-end">
              <button className="ml-2" onClick={this.confirmCreate} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
              <button className="ml-2" onClick={this.cancelCreate}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
            </div>
          </div>
        }
      </div>
    )
  }
}