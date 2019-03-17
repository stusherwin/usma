import * as React from 'react';

import { Household, HouseholdPayment, CollectiveOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { Form, Field, Validate } from '../common/Validation'
import { TextField, MoneyField } from '../common/Field'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface HouseholdPaymentsState { editing: 'new' | number | null
                                        , form: Form
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, HouseholdPaymentsState> {
  constructor(props: HouseholdPaymentsProps) {
    super(props)

    this.state = { editing: null
                 , form: Form.create({ date: Field.create((v: string) => new Date(Date.parse(v)), (v: Date) => Util.dateString(v),
                                         [ Validate.required('Date is required')
                                         , Validate.dateFormat('Date must be in the format YYYY-MM-DD')
                                         , Validate.dateExists('Date must actually exist')
                                         ])
                                     , amount: Field.create((v: string) => Math.floor((parseFloat(v) || 0) * 100), (v: number) => (v / 100.0).toFixed(2),
                                         [ Validate.required('Amount is required')
                                         , Validate.decimal('Amount must be a number')
                                         , Validate.twoDP('Amount can\'t have more than 2 decimal places')
                                         , Validate.greaterThanZero('Amount must be more than zero')
                                         ])
                                     })
                 }
  }
  
  startCreate = () => this.setState({ editing: 'new'
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({date: new Date(), amount: 0})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.createHouseholdPayment(this.props.household.id, validated.fields.date.value, validated.fields.amount.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({date: new Date(), amount: 0})
                                 }))
    }
  }
  
  startEdit = (payment: HouseholdPayment) => this.setState({ editing: payment.id
                                                           , form: this.state.form.reset({date: payment.date, amount: payment.amount})
                                                           })

  cancelEdit = () => this.setState({ editing: null
                                   , form: this.state.form.reset({date: new Date(), amount: 0})
                                   })

  confirmEdit = () => {
    if(typeof this.state.editing !== 'number') return

    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHouseholdPayment(this.state.editing, validated.fields.date.value, validated.fields.amount.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({date: new Date(), amount: 0})
                                 }))
    }
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })
  
  delete = (p: HouseholdPayment) => {
    this.props.request(ServerApi.command.archiveHouseholdPayment(p.id))
      .then(this.props.reload)
  }

  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)

    return (
      <div>
        <div className="bg-payment-light p-2">
          <div className="bg-img-payment bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-payment-dark leading-none mb-2 -mt-1">Payments</h2>
            <div className="flex justify-start">
              <button onClick={this.startCreate} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New payment</button>
            </div>
          </div>
        </div>
        {this.state.editing == 'new' &&
          <div className="bg-payment-lightest p-2">
            <h3 className="mb-4">Create new payment</h3>
            <TextField id="create-date"
                       label="Date"
                       field={this.state.form.fields.date}
                       valueOnChange={this.fieldChanged('date')} />
            <MoneyField id="create-amount"
                        label="Amount"
                        field={this.state.form.fields.amount}
                        valueOnChange={this.fieldChanged('amount')} />
            <div className="flex justify-end">
              <button className="ml-2" onClick={this.confirmCreate} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
              <button className="ml-2" onClick={this.cancelCreate}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
            </div>
          </div>
        }
        {!this.props.payments.length && !this.state.editing &&
          <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
        }
        {!!this.props.payments.length &&
          <table className="border-collapse w-full mb-4">
            <tbody>
              { this.props.payments.map(p =>
              this.state.editing == p.id
              ? (
                <tr key={p.id}>
                  <td colSpan={3} className="bg-payment-lightest p-2">
                    <h3 className="mb-4">Edit payment</h3>
                    <TextField id="edit-date"
                               label="Date"
                               field={this.state.form.fields.date}
                               valueOnChange={this.fieldChanged('date')} />
                    <MoneyField id="edit-amount"
                                label="Amount"
                                field={this.state.form.fields.amount}
                                valueOnChange={this.fieldChanged('amount')} />
                    <div className="flex justify-end">
                      <button className="ml-2" onClick={this.confirmEdit} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
                      <button className="ml-2" onClick={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
                    </div>
                  </td>
                </tr>
              )
              : (
                <tr key={p.id}>
                  <td className="pt-2 pl-2 pr-2 w-full">{ Util.formatDate(p.date) }</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={p.amount} /></td>
                  <td className="pt-2 pr-2 w-1 whitespace-no-wrap">
                    <button onClick={_ => this.startEdit(p)} disabled={!!this.state.editing}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></button>
                    <button className="ml-2" onClick={_ => this.delete(p)} disabled={!!this.state.editing}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                  </td>
                </tr>
              )) }
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={total} /></td>
                <td className="pt-2 pr-2"></td>
              </tr>
            </tbody>
          </table>
        }
      </div>
    )
  }
}