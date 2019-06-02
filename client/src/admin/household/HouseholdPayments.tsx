import * as React from 'react';

import { Household, HouseholdPayment } from '../../Types'
import { ServerApi } from '../../ServerApi'
import { Util } from '../../common/Util'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { HouseholdPaymentForm, PaymentData } from './HouseholdPaymentForm'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface HouseholdPaymentsState { editing: 'new' | number | null
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, HouseholdPaymentsState> {
  constructor(props: HouseholdPaymentsProps) {
    super(props)

    this.state = { editing: null }
  }
  
  startCreate = () => this.setState({ editing: 'new' })

  cancelCreate = () => this.setState({ editing: null })

  confirmCreate = ({date, amount}: PaymentData) => 
      this.props.request(ServerApi.command.createHouseholdPayment(this.props.household.id, date, amount))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null }))
  
  startEdit = (payment: HouseholdPayment) => this.setState({ editing: payment.id })

  cancelEdit = () => this.setState({ editing: null })

  confirmEdit = ({date, amount}: PaymentData) => {
    if(typeof this.state.editing !== 'number')
      return Promise.resolve()

    return this.props.request(ServerApi.command.updateHouseholdPayment(this.state.editing, date, amount))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null }))
  }

  delete = (p: HouseholdPayment) => {
    this.props.request(ServerApi.command.archiveHouseholdPayment(p.id))
      .then(this.props.reload)
  }

  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)

    return (
      <div>
        <div className="bg-payment-light p-2 mt-4">
          <div className="bg-img-payment bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-payment-dark leading-none mb-2 -mt-1">Payments</h2>
            <div className="flex justify-start">
              <button onClick={this.startCreate} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New payment</button>
            </div>
          </div>
        </div>
        {this.state.editing == 'new' &&
          <HouseholdPaymentForm key="create"
                                onCancel={this.cancelCreate}
                                onConfirm={this.confirmCreate}>
          </HouseholdPaymentForm>
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
                  <td colSpan={3}>
                    <HouseholdPaymentForm key="edit"
                                          payment={p}
                                          onCancel={this.cancelEdit}
                                          onConfirm={this.confirmEdit}>
                    </HouseholdPaymentForm>
                  </td>
                </tr>
              )
              : (
                <tr key={p.id}>
                  <td className="pt-2 pl-2 pr-2 w-full">{ Util.formatDate(p.date) }</td>
                  <td className="pt-2 pr-2 text-right"><Money amount={-p.amount} /></td>
                  <td className="pt-2 pr-2 w-1 whitespace-no-wrap">
                    <button onClick={_ => this.startEdit(p)} disabled={!!this.state.editing}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></button>
                    <button className="ml-2" onClick={_ => this.delete(p)} disabled={!!this.state.editing}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                  </td>
                </tr>
              )) }
              <tr>
                <td className="pt-2 pl-2 pr-2 font-bold">Total</td>
                <td className="pt-2 pr-2 font-bold text-right"><Money amount={-total} /></td>
                <td className="pt-2 pr-2"></td>
              </tr>
            </tbody>
          </table>
        }
      </div>
    )
  }
}