import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdPayment } from 'util/Types'
import { Util } from 'util/Util'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'

import { HouseholdPaymentForm, PaymentData } from './HouseholdPaymentForm'

export interface HouseholdPaymentsProps { household: Household
                                        , readOnly?: boolean
                                        , collapsibleKey: string
                                        , collapsibleState: CollapsibleState
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
    const total = this.props.household.householdPayments.reduce((tot, p) => tot + p.amount, 0)
 
    return (
      <Collapsible className="min-h-20"
                   collapsibleKey={this.props.collapsibleKey}
                   collapsibleState={this.props.collapsibleState}
                   {...this.props}
                   header={
                     <div className="p-2 bg-payment-light min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-payment"></div>
                       <div className="flex justify-between">
                         <h2 className="leading-none ml-20">
                           Payments
                         </h2>
                         <h3>
                           <Money className="text-right" amount={-this.props.household.totalPayments} noColour />
                         </h3>
                       </div>
                     </div>
                   }
                   expandedHeader={!this.props.readOnly && 
                     <div className="p-2 bg-payment-light flex justify-start">
                       <button onClick={e => { e.preventDefault(); e.stopPropagation(); this.startCreate() }} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New payment</button>
                     </div>
                   || undefined}>
        <div className="shadow-inner-top bg-white">
          {this.state.editing == 'new' &&
            <HouseholdPaymentForm key="create"
                                  onCancel={this.cancelCreate}
                                  onConfirm={this.confirmCreate}>
            </HouseholdPaymentForm>
          }
          {!this.props.household.householdPayments.length && !this.state.editing &&
            <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 ml-20 mr-2 fill-current nudge-d-2" />No payments made
            </div>
          }
          {!!this.props.household.householdPayments.length &&
            <table className="border-collapse w-full">
              <tbody>
                { this.props.household.householdPayments.map((p, i) =>
                  this.state.editing == p.id
                  ? (
                    <tr key={p.id}>
                      <td colSpan={3} className={classNames({"pt-4": i > 0})}>
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
                    <td className={classNames('pt-4 pl-20 pr-2 whitespace-no-wrap')}><span className="pl-2">{ Util.formatDate(p.date) }</span></td>
                    {!this.props.readOnly && 
                      <td className={classNames("pt-4 pr-2 w-full whitespace-no-wrap")}>
                        <button onClick={_ => this.startEdit(p)} disabled={!!this.state.editing}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></button>
                        <button className="ml-2" onClick={_ => this.delete(p)} disabled={!!this.state.editing}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></button>
                      </td>
                    }
                    <td className={classNames('pt-4 pr-2 text-right whitespace-no-wrap')}><Money amount={-p.amount} noColour /></td>
                  </tr>
                  )
                ) }
                <tr>
                  <td className="pt-4 pl-20 pr-2 pb-4 font-bold" colSpan={this.props.readOnly? 2 : 3}>
                    <div className="flex justify-between">
                      <span className="pl-2">Total:</span>
                      <span className={classNames("font-bold text-right whitespace-no-wrap")}><Money amount={-total} noColour /></span>
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>
          }
        </div>
      </Collapsible>
    )
  }
}