import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

const transitionTime = 0.25;
const minHeight = '5rem';

export interface PastHouseholdOrdersProps { householdOrders: PastHouseholdOrder[]
                                          , expanded: boolean
                                          , otherExpanding: boolean
                                          , toggle: () => void
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export interface HouseholdPaymentsState { expanded: PastHouseholdOrder | null
                                        }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, HouseholdPaymentsState> {
  container: React.RefObject<HTMLDivElement>

  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.container = React.createRef();

    this.state = {
      expanded: null,
    }
  }

  componentDidUpdate(prevProps: PastHouseholdOrdersProps) {
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

  expandOrder = (ho: PastHouseholdOrder) => {
    if(this.state.expanded == ho) {
      this.setState({expanded: null})
    } else {
      this.setState({expanded: ho})
    }
  }

  render() {
    const pastOrders = this.props.householdOrders
    const total = this.props.householdOrders.filter(ho => !ho.isAbandoned).reduce((tot, ho) => tot + ho.totalIncVat, 0)
    const itemCount = (ho: PastHouseholdOrder) => {
      const sum = ho.items.reduce((tot, ho) => tot + ho.itemQuantity, 0)
      return sum + (sum == 1 ? ' item' : ' items')
    }

    return (
      <div ref={this.container} className="min-h-20" style={{ 
            overflow: 'hidden',
            height: minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.props.expanded? '0s' : (this.props.otherExpanding? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.unsetHeight}>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className={classNames(
            'bg-past-order-lighter p-2 block no-underline hover:no-underline text-black hover:text-black min-h-20', {
            })}>
          <div className="bg-img-order bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Past orders
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
          <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span><span><Money amount={total} /></span></h3>
        </a>
        <div className="shadow-inner-top bg-white py-4">
          { !pastOrders.length
            ? <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders</div>
            : (
              <table className="border-collapse w-full">
                <tbody>
                  { pastOrders.map((ho, i) => ([
                      <tr key={ho.orderId} className={classNames({'crossed-out': ho.isAbandoned})}>
                        <td className={classNames('pl-2 pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}>
                          <a href="#" onClick={e => {e.preventDefault(); this.expandOrder(ho)}}>{Util.formatDate(ho.orderCreatedDate)}</a>
                          <Icon type={this.state.expanded? 'collapse' : 'expand'} className="w-3 h-3 ml-2 text-grey-dark fill-current" />
                        </td>
                        {/* <td className={classNames('pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}>{itemCount(ho)}</td> */}
                        <td className={classNames('pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}>{ho.isAbandoned && 'Abandoned'}</td>
                        <td className={classNames('pr-2 text-right', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}><Money amount={ho.totalIncVat} /></td>
                      </tr>
                      ,
                      this.state.expanded == ho &&
                        <tr>
                          <td colSpan={3}>
                            <table>
                              <tbody>
                                {ho.items.map(i =>
                                  <tr key={i.productId} className={classNames({'crossed-out': ho.isAbandoned})}>  
                                    <td className="bg-grey-lighter pt-2 pl-2 pr-2">{i.productCode}</td>
                                    <td className="bg-grey-lighter pt-2 pr-2 w-full">{i.productName}</td>
                                    <td className="bg-grey-lighter pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                                    <td className="bg-grey-lighter pt-2 pr-2 text-right"><Money amount={i.itemTotalExcVat} /></td>
                                  </tr>
                                )}
                                <tr className={classNames({'crossed-out': ho.isAbandoned})}>
                                  <td className="bg-grey-lighter pt-2 pl-2 pr-2">VAT:</td>
                                  <td className="bg-grey-lighter pt-2 pr-2"></td>
                                  <td className="bg-grey-lighter pt-2 pr-2"></td>
                                  <td className="bg-grey-lighter pt-2 pr-2 text-right"><Money amount={ho.totalIncVat - ho.totalExcVat} /></td>
                                </tr>
                                <tr className={classNames({'crossed-out': ho.isAbandoned})}>
                                  <td className="bg-grey-lighter pt-2 pb-2 pl-2 pr-2 font-bold">Total:</td>
                                  <td className="bg-grey-lighter pt-2 pb-2 pr-2"></td>
                                  <td className="bg-grey-lighter pt-2 pb-2 pr-2"></td>
                                  <td className="bg-grey-lighter pt-2 pb-2 pr-2 font-bold text-right"><Money amount={ho.totalIncVat} /></td>
                                </tr>
                              </tbody>
                            </table>
                          </td>
                        </tr>
                      ]
                  )) }
                  <tr>
                    <td className="pt-2 pl-2 pr-2 font-bold">Total:</td>
                    {/* <td className="pt-2 pb-2 pr-2"></td> */}
                    <td className="pt-2 pb-2 pr-2"></td>
                    <td className="pt-2 pr-2 font-bold text-right"><Money amount={total} /></td>
                  </tr>
                </tbody>
              </table>
            )
          }
        </div>
      </div>
    )
  }
}