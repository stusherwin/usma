import * as React from 'react';

import { Household } from '../Types'
import { Form, Field, Validate } from '../common/Validation'
import { TextField } from '../common/Field'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'

export interface EditHouseholdProps { household: Household
                                    , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                    , onConfirm: () => void
                                    , onCancel: () => void
                                    }
export interface EditHouseholdState { form: Form
                                    , editFocused: boolean
                                    }

export class EditHousehold extends React.Component<EditHouseholdProps, EditHouseholdState> {  
  nameInput: React.RefObject<HTMLInputElement>

  constructor(props: EditHouseholdProps) {
    super(props)
    this.state = { 
      editFocused: false,
      form: Form.create({ 
        name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]),
        contactName: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactEmail: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactPhone: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', [])
      })
    }
    this.nameInput = React.createRef();
  }
  
  confirmEdit = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.props.household.id, validated.fields.name.value, validated.fields.contactName.value, validated.fields.contactEmail.value, validated.fields.contactPhone.value))
        .then(this.props.onConfirm)
    }
  }

  cancelEdit = () => {
    this.props.onCancel();
  }

  reset = () => { 
    this.setState({ form: this.state.form.reset({ name: this.props.household.name
                                                , contactName: this.props.household.contactName
                                                , contactEmail: this.props.household.contactEmail
                                                , contactPhone: this.props.household.contactPhone
                                                })
                  })
  }

  fieldChanged = (fieldName: string) => (value: string) => {
    this.setState({ form: this.state.form.update(fieldName, value) })
  }

  focus = () => {
    if(this.nameInput.current) { 
      this.nameInput.current.focus()
    }
  }

  blur = () => {
    if(this.nameInput.current) { 
      this.nameInput.current.blur()
    }
  }

  render() {
    return (
      <div className="shadow-inner-top px-2 py-4 bg-household-lightest">
        <h3 className="mb-4">Edit household</h3>
        <TextField id="edit-name"
                   label="Name"
                   inputRef={this.nameInput}
                   field={this.state.form.fields.name}
                   valueOnChange={this.fieldChanged('name')} />
        <TextField id="edit-contactName"
                   label="Contact name"
                   field={this.state.form.fields.contactName}
                   valueOnChange={this.fieldChanged('contactName')} />
        <TextField id="edit-contactEmail"
                   label="Contact email"
                   field={this.state.form.fields.contactEmail}
                   valueOnChange={this.fieldChanged('contactEmail')} />
        <TextField id="edit-contactPhone"
                   label="Contact phone"
                   field={this.state.form.fields.contactPhone}
                   valueOnChange={this.fieldChanged('contactPhone')} />
        <div className="flex justify-end">
          <button className="ml-2" onClick={this.confirmEdit} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
          <button className="ml-2" onClick={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
        </div>
      </div>
    )
  }
}

