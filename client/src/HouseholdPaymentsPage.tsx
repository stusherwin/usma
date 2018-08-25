import * as React from 'react';

import { Household, HouseholdPayment, CollectiveOrder } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link, RouterLink } from './Link'
import { Money } from './Money'
import { Router } from './Router'
import { Form, Field, Validate } from './Validation'

export interface HouseholdPaymentsPageProps { household: Household
                                            , payments: HouseholdPayment[]
                                            , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                            , reload: () => void
                                            }

export interface HouseholdPaymentsPageState { creating: boolean 
                                            , form: Form
                                            }

export class HouseholdPaymentsPage extends React.Component<HouseholdPaymentsPageProps, HouseholdPaymentsPageState> {
  constructor(props: HouseholdPaymentsPageProps) {
    super(props)

    this.state = { creating: false
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
        .then(products => {
          this.setState({ creating: false
                        , form: this.state.form.reset({date: new Date(), amount: 0})
                        })
          this.props.reload()
        })
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
        <div><RouterLink path="/households">Households</RouterLink> &gt;</div>
        <h1>{this.props.household.name}</h1>
        <div>
          <RouterLink path={`/households/${this.props.household.id}/orders`}>Orders</RouterLink>
          <div>Payments</div>
          
          {!this.state.creating && 
            <Link action={this.startCreate}>New payment</Link>
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
              <Link action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Link>
              <Link action={this.cancelCreate}>Cancel</Link>
            </div>
          }

          {!this.props.payments.length 
          ? <div>No payments yet</div>
          : (
            <div>
              { this.props.payments.map(p => (
                <div key={p.id}>
                  <span>{ Util.formatDate(p.date) }</span>
                  <Money amount={p.amount} />
                  <Link action={() => this.delete(p)}>Delete</Link>
                </div>
              )) }
            </div>
          )}

          <div>
            <span>Total:</span>
            <Money amount={total} />
          </div>
        </div>
      </div>
    )
  }
}