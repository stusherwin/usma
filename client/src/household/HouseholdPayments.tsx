import * as React from 'react';

import { Household, HouseholdPayment } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , expanded: boolean
                                        , toggle: () => void
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface HouseholdPaymentsState { height: number }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, HouseholdPaymentsState> {
  content: React.RefObject<HTMLDivElement>

  constructor(props: HouseholdPaymentsProps) {
    super(props)

    this.content = React.createRef();
    this.state = { height: 0 }
  }

  componentWillReceiveProps() {
    if(!this.content.current) return

    this.setState({height: this.content.current.scrollHeight})
  }

  componentDidMount() {
    if(!this.content.current) return

    this.setState({height: this.content.current.scrollHeight})
  }

  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)
 
    return (
      <div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className="bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark">
          <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mr-2" />
          <div className="bg-img-payment bg-no-repeat bg-16 pl-20 min-h-16 relative">
            <h2 className="text-payment-dark leading-none mb-2">Payments</h2>
            <h3 className="flex justify-between"><span>Total payments:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
          </div>
        </a>
        <div ref={this.content} className="transition-height" style={{height: this.props.expanded? this.state.height : 0}}>        
          { this.props.payments.length 
              ? <table className="border-collapse w-full mb-4">
                  <tbody>
                    { this.props.payments.map(p =>
                      <tr key={p.id}>
                        <td className="pt-2 pl-2 pr-2 w-full">{ Util.formatDate(p.date) }</td>
                        <td className="pt-2 pr-2 text-right"><Money amount={p.amount} /></td>
                      </tr>
                    ) }
                    <tr>
                      <td className="pt-2 pl-2 pr-2 font-bold">Total payments</td>
                      <td className="pt-2 pr-2 font-bold text-right"><Money amount={total} /></td>
                    </tr>
                  </tbody>
                </table>
              : <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
          }
        </div>
      </div>
    )
  }
}