import * as React from 'react';

import { Household, HouseholdPayment, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Button } from './Button'
import { Money } from './Money'
import { Router } from './Router'
import { Form, Field, Validate } from './Validation'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface HouseholdPaymentsState { creating: boolean 
                                        , editingId: number | null
                                        , form: Form
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, HouseholdPaymentsState> {
  constructor(props: HouseholdPaymentsProps) {
    super(props)

    this.state = { creating: false
                 , editingId: null
                 , form: Form.create({ date: Field.create((v: string) => new Date(Date.parse(v)), (v: Date) => Util.dateString(v),
                                         [ Validate.required('Date is required')
                                         , Validate.dateFormat('Date must be in the format YYYY-MM-DD')
                                         , Validate.dateExists('Date must actually exist')
                                         ])
                                     , amount: Field.create((v: string) => (parseFloat(v) || 0) * 100, (v: number) => (v / 100.0).toFixed(2),
                                         [ Validate.required('Amount is required')
                                         , Validate.decimal('Amount must be a number')
                                         , Validate.twoDP('Amount can\'t have more than 2 decimal places')
                                         , Validate.greaterThanZero('Amount must be more than zero')
                                         ])
                                     })
                 }
  }
  
  startCreate = () => this.setState({ creating: true
                                    })

  cancelCreate = () => this.setState({ creating: false
                                     , form: this.state.form.reset({date: new Date(), amount: 0})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.createHouseholdPayment(this.props.household.id, validated.fields.date.value, validated.fields.amount.value))
        .then(this.props.reload)
        .then(_ => this.setState({ creating: false
                                 , form: this.state.form.reset({date: new Date(), amount: 0})
                                 }))
    }
  }
  
  startEdit = (payment: HouseholdPayment) => this.setState({ editingId: payment.id
                                                           , form: this.state.form.reset({date: payment.date, amount: payment.amount})
                                                           })

  cancelEdit = () => this.setState({ editingId: null
                                   , form: this.state.form.reset({date: new Date(), amount: 0})
                                   })

  confirmEdit = () => {
    if(!this.state.editingId) return

    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHouseholdPayment(this.state.editingId, validated.fields.date.value, validated.fields.amount.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editingId: null
                                 , form: this.state.form.reset({date: new Date(), amount: 0})
                                 }))
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: this.state.form.update(fieldName, event.target.value) })
  
  delete = (p: HouseholdPayment) => {
    this.props.request(ServerApi.command.archiveHouseholdPayment(p.id))
      .then(this.props.reload)
  }

  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)

    return (
      <div>
        <h2>Payments</h2>
        
        {!this.state.creating && 
          <Button action={this.startCreate}>New payment</Button>
        }
        {this.state.creating &&
          <div>
            <span>
              <input type="text" value={this.state.form.fields.date.stringValue} className={!this.state.form.fields.date.valid? 'invalid': 'valid'} onChange={this.fieldChanged('date')} />
              {this.state.form.fields.date.error}
            </span>
            <span>
              <input type="text" value={this.state.form.fields.amount.stringValue} className={!this.state.form.fields.amount.valid? 'invalid': 'valid'} onChange={this.fieldChanged('amount')} />
              {this.state.form.fields.amount.error}
            </span>
            <Button action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Button>
            <Button action={this.cancelCreate}>Cancel</Button>
          </div>
        }

        {!this.props.payments.length 
        ? <div>No payments yet</div>
        : (
          <div>
            { this.props.payments.map(p =>
            this.state.editingId == p.id
            ? (
              <div>
                <span>
                  <input type="text" value={this.state.form.fields.date.stringValue} className={!this.state.form.fields.date.valid? 'invalid': 'valid'} onChange={this.fieldChanged('date')} />
                  {this.state.form.fields.date.error}
                </span>
                <span>
                  <input type="text" value={this.state.form.fields.amount.stringValue} className={!this.state.form.fields.amount.valid? 'invalid': 'valid'} onChange={this.fieldChanged('amount')} />
                  {this.state.form.fields.amount.error}
                </span>
                <Button action={this.confirmEdit} disabled={!this.state.form.valid()}>Save</Button>
                <Button action={this.cancelEdit}>Cancel</Button>
              </div>
            )
            : (
              <div key={p.id}>
                <span>{ Util.formatDate(p.date) }</span>
                <Money amount={p.amount} />
                <Button action={() => this.startEdit(p)}>Edit</Button>
                <Button action={() => this.delete(p)}>Delete</Button>
              </div>
            )) }
          </div>
        )}

        <div>
          <span>Total:</span>
          <Money amount={total} />
        </div>
      </div>
    )
  }
}