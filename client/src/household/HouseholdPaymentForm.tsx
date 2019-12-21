import * as React from 'react';

import { HouseholdPayment } from '../util/Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Form, Field, Validate } from '../common/Validation'
import { TextField, MoneyField } from '../common/Field'

export interface PaymentData {
  date: Date
  amount: number
}

export interface HouseholdPaymentFormProps { payment?: HouseholdPayment
                                           , key: string
                                           , onCancel: () => void
                                           , onConfirm: (data: PaymentData) => Promise<void>
                                           }

export interface HouseholdPaymentFormState { form: Form
                                           }

export class HouseholdPaymentForm extends React.Component<HouseholdPaymentFormProps, HouseholdPaymentFormState> {
  constructor(props: HouseholdPaymentFormProps) {
    super(props)

    this.state = { form: Form.create({ date: Field.create((v: string) => new Date(Date.parse(v)), (v: Date) => Util.dateString(v),
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

  componentDidMount() {
    this.reset()
  }
  
  cancel = () => {
    this.reset()
    this.props.onCancel()
  }

  confirm = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.onConfirm({
        date: validated.fields.date.value, 
        amount: validated.fields.amount.value
      }).then(_ => this.reset())
    }
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })

  reset = () => {
    this.setState({ form: this.state.form.reset(this.props.payment? {date: this.props.payment.date, amount: this.props.payment.amount}: {date: new Date(), amount: ''})
                  })
  }
  
  render() {
    return (
      <div className="bg-payment-lightest px-2 py-4 shadow-inner-top">
        <h3 className="mb-4">{this.props.payment ? 'Edit payment' : 'Create new payment'}</h3>
        <TextField id={`${this.props.key}-date`}
                   label="Date"
                   field={this.state.form.fields.date}
                   autofocus
                   valueOnChange={this.fieldChanged('date')} />
        <MoneyField id={`${this.props.key}-amount`}
                    label="Amount"
                    field={this.state.form.fields.amount}
                    valueOnChange={this.fieldChanged('amount')} />
        <div className="flex justify-end">
          <button className="ml-2" onClick={this.confirm} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
          <button className="ml-2" onClick={this.cancel}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
        </div>
      </div>
    )
  }
}
