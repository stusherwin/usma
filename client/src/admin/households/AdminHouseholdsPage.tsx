import * as React from 'react';
import * as classNames from 'classnames'

import { Household, GroupSettings } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { RouterLink } from 'util/RouterLink'
import { BalanceSmall } from 'util/Money'
import { Icon } from 'util/Icon'
import { Form, Field, Validate } from 'util/Validation'
import { TextField } from 'util/Field'

import { AdminTopNav } from 'admin/AdminTopNav'

export interface AdminHouseholdsPageProps { households: Household[]
                                            groupSettings: GroupSettings
                                            request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            reload: () => Promise<void>
                                          }

export interface AdminHouseholdsPageState { editing: 'new' | number | null
                                            form: Form
                                          }

export class AdminHouseholdsPage extends React.Component<AdminHouseholdsPageProps, AdminHouseholdsPageState> {
  constructor(props: AdminHouseholdsPageProps) {
    super(props)

    this.state = { editing: null
                 , form: Form.create({ 
                     name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]),
                     contactName: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
                     contactEmail: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
                     contactPhone: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', [])
                   })
                 }
  }

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
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                 })
        )
    }
  }

  startEdit = (household: Household) => this.setState({ editing: household.id
                                                      , form: this.state.form.reset({name: household.name, contactName: household.contactName, contactEmail: household.contactEmail, contactPhone: household.contactPhone})
                                                      })

  cancelEdit = () => this.setState({ editing: null
                                   , form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                   })

  confirmEdit = () => {
    if(typeof this.state.editing !== 'number') return

    const validated = this.state.form.validate()
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.state.editing, validated.fields.name.value, validated.fields.contactName.value, validated.fields.contactEmail.value, validated.fields.contactPhone.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                 })
        )
    }
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })

  delete = (h: Household) => 
    this.props.request(ServerApi.command.archiveHousehold(h.id))
      .then(this.props.reload)

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        <AdminTopNav />
        <div className="p-2 text-black min-h-24">
          <div className="bg-img-household bg-no-repeat w-16 h-16 absolute mt-2"></div>
          <h2 className="text-household-darker leading-none ml-20 mt-2 relative flex">Households</h2>
          <div className="flex justify-start ml-20 mt-2 mb-2">
            <button onClick={this.startCreate} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New household</button>
          </div>
        </div>
        <div className="bg-household-lighter shadow-inner-top">
          {this.state.editing == 'new' &&
            <div className="bg-household-lightest shadow-inner-top px-2 py-4">
              <h3 className="mb-4">Create new household</h3>
              <TextField id="create-name"
                         label="Name"
                         autofocus
                         field={this.state.form.fields.name}
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
          {!this.props.households.length && !this.state.editing
          ? <div className="p-2 mb-4 text-black"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households created yet</div>
          : (
            <div>
              { this.props.households.map((h, i) => 
              this.state.editing == h.id
              ? (
                <div key={h.id} className={classNames('bg-household-lightest shadow-inner-top px-2 py-4', {'mt-4': i > 0})}>
                  <h3 className="mb-4">Edit household</h3>
                  <TextField id="edit-name"
                             label="Name"
                             autofocus
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
              : (
               <RouterLink className={classNames("p-2 py-4 min-h-24 block no-underline text-black hover:underline hover:text-black relative hover:bg-household-lightish", {"shadow-inner-top": i == 0})} path={`/admin/households/${h.id}`}>
                 <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                 <div className="ml-20 flex items-baseline justify-between">
                   <div>
                     <h3 className="leading-none">
                       {h.name}<span className="-ml-3 pl-4 whitespace-no-wrap"><Icon type="right-arrow" className="text-black inline w-3 h-3 fill-current nudge-d-1" /></span>
                     </h3>
                     <div className="mt-4 text-base inline-block"><strong>Contact:</strong> {h.contactName || 'none'}</div>
                   </div>
                   <div className="flex flex-col items-end">
                     {this.props.groupSettings.enablePayments &&    
                       <BalanceSmall className="text-right py-1 px-1 -mr-1" amount={-h.balance} />
                     }
                     <button className={classNames({"mt-2": this.props.groupSettings.enablePayments})} onClick={_ => this.delete(h)} disabled={!!this.state.editing}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                   </div>
                 </div>
               </RouterLink>
              )) }
            </div>
          )}
        </div>
      </div>
    )
  }
}