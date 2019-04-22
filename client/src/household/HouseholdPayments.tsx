import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdPayment } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , expanded: boolean
                                        , otherExpanding: boolean
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
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className={classNames(
            'bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark', {
              'min-h-0': !this.props.expanded,
              'min-h-20': this.props.expanded 
            })} style={{ 
              transition: this.props.expanded
                // expanding
                ? 'min-height 0.125s ease-in 0s' //(this.props.otherExpanding? 'min-height 0.125s ease-in 0.25s' : 'min-height 0.125s ease-in 0s')
                // collapsing
                : (this.props.otherExpanding? 'min-height 0.125s ease-out 0.375s' : 'min-height 0.125s ease-out 0.125s')
            }}>
          <div className="bg-img-payment bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Payments
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
        </a>
        <div ref={this.content} style={{
            overflow: 'hidden',
            transition: this.props.expanded
              // expanding
              ? 'height 0.125s ease-out 0.125s' //(this.props.otherExpanding ? 'height 0.125s ease-out 0.375s' : 'height 0.125s ease-out 0.125s')
              // collapsing
              : (this.props.otherExpanding? 'height 0.125s ease-in 0.25s' : 'height 0.125s ease-in 0s'),
            height: this.props.expanded? this.state.height : 0,
            boxShadow: 'rgba(0, 0, 0, 0.1) 0px 5px 5px 0px inset'
          }}>
          { this.props.payments.length 
              ? <table className="border-collapse w-full mb-4 mt-2">
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
              : <div className="p-2 mb-2 mt-2 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
          }
        </div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className="bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark">
          <h3 className="flex justify-between ml-20"><span>Total:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
        </a>
      </div>
    )
  }
}