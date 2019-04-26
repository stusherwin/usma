import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdPayment } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

const transitionTime = 0.25;
const minHeight = '5rem';

export interface HouseholdPaymentsProps { household: Household
                                        , payments: HouseholdPayment[]
                                        , expanded: boolean
                                        , otherExpanding: boolean
                                        , toggle: () => void
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export class HouseholdPayments extends React.Component<HouseholdPaymentsProps, {}> {
  container: React.RefObject<HTMLDivElement>

  constructor(props: HouseholdPaymentsProps) {
    super(props)

    this.container = React.createRef();
  }

  componentDidUpdate(prevProps: HouseholdPaymentsProps) {
    if(prevProps.expanded != this.props.expanded) {
      this.animateHeight()
    }
  }

  componentDidMount() {
    this.animateHeight()
  }

  animateHeight() {
    const el = this.container.current
    if(!el) return

    if(this.props.expanded) {
      el.style.height = el.scrollHeight + 'px';
    } else {
      el.style.height = el.scrollHeight + 'px';
      el.offsetHeight; // trigger reflow
      el.style.height = minHeight;
    }
  }

  unsetHeight = () => {
    const el = this.container.current
    if(!el) return

    if(this.props.expanded) {
      el.style.height = null;
    }
  }
  
  render() {
    const total = this.props.payments.reduce((tot, p) => tot + p.amount, 0)
 
    return (
      <div ref={this.container} className="min-h-20" style={{ 
            overflow: 'hidden',
            height: minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.expanded? '0s' : (this.props.otherExpanding? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.unsetHeight}>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className={classNames(
            'bg-payment-light p-2 block no-underline hover:no-underline text-payment-dark hover:text-payment-dark min-h-20', {
            })}>
          <div className="bg-img-payment bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Payments
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
          <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span><span><Money amount={this.props.household.totalPayments} /></span></h3>
        </a>
        <div className="shadow-inner-top bg-white px-2 py-4">
          { this.props.payments.length 
            ? <table className="border-collapse w-full">
                <tbody>
                  { this.props.payments.map((p, i) =>
                    <tr key={p.id}>
                      <td className={classNames('pr-2 w-full', {'pt-2': i > 0})}>{ Util.formatDate(p.date) }</td>
                      <td className={classNames('text-right', {'pt-2': i > 0})}><Money amount={p.amount} /></td>
                    </tr>
                  ) }
                  <tr>
                    <td className="pt-2 pr-2 font-bold">Total:</td>
                    <td className="pt-2 font-bold text-right"><Money amount={total} /></td>
                  </tr>
                </tbody>
              </table>
            : <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No payments yet</div>
          }
        </div>
      </div>
    )
  }
}