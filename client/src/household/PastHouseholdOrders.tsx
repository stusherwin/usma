import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'

export interface PastHouseholdOrdersProps { householdOrders: PastHouseholdOrder[]
                                          , expanded: boolean
                                          , otherExpanding: boolean
                                          , toggle: () => void
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export interface HouseholdPaymentsState { height: number 
                                        , expanded: PastHouseholdOrder | null
                                        }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, HouseholdPaymentsState> {
  content: React.RefObject<HTMLDivElement>

  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.content = React.createRef();

    this.state = {
      expanded: null,
      height: 0
    }
  }

  componentWillReceiveProps() {
    if(!this.content.current) return

    this.setState({height: this.content.current.scrollHeight})
  }

  componentDidMount() {
    if(!this.content.current) return

    this.setState({height: this.content.current.scrollHeight})
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
      <div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className={classNames(
            'bg-past-order-lighter p-2 block no-underline hover:no-underline text-black hover:text-black', {
              'min-h-0': !this.props.expanded,
              'min-h-20': this.props.expanded 
            })} style={{ 
              transition: this.props.expanded
                // expanding
                ? 'min-height 0.125s ease-in 0s' //(this.props.otherExpanding? 'min-height 0.125s ease-in 0.25s' : 'min-height 0.125s ease-in 0s')
                // collapsing
                : (this.props.otherExpanding? 'min-height 0.125s ease-out 0.375s' : 'min-height 0.125s ease-out 0.125s')
            }}>
          <div className="bg-img-order bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Past orders
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
          { !pastOrders.length
            ? <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders</div>
            : (
              <table className="border-collapse w-full mb-4 mt-2">
                <tbody>
                  { pastOrders.map(ho => ([
                      <tr key={ho.orderId} className={classNames({'crossed-out': ho.isAbandoned})}>
                        <td className={classNames('pt-2 pl-2 pr-2', {'pb-2': this.state.expanded == ho})}><a href="#" onClick={e => {e.preventDefault(); this.expandOrder(ho)}}>{Util.formatDate(ho.orderCreatedDate)}</a></td>
                        <td className={classNames('pt-2 pr-2', {'pb-2': this.state.expanded == ho})}>{itemCount(ho)}</td>
                        <td className={classNames('pt-2 pr-2', {'pb-2': this.state.expanded == ho})}>{ho.isAbandoned && 'Abandoned'}</td>
                        <td className={classNames('pt-2 pr-2 text-right', {'pb-2': this.state.expanded == ho})}>{this.state.expanded != ho && <Money amount={ho.totalIncVat} />}</td>
                      </tr>
                      ,
                      this.state.expanded == ho &&
                        <tr>
                          <td colSpan={4}>
                            <table>
                              <tbody>
                                {ho.items.map(i =>
                                  <tr key={i.productId}>  
                                    <td className="bg-grey-lightest pt-2 pl-2 pr-2">{i.productCode}</td>
                                    <td className="bg-grey-lightest pt-2 pr-2 w-full">{i.productName}</td>
                                    <td className="bg-grey-lightest pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                                    <td className="bg-grey-lightest pt-2 pr-2 text-right"><Money amount={i.itemTotalExcVat} /></td>
                                  </tr>
                                )}
                                <tr>
                                  <td className="bg-grey-lightest pt-2 pl-2 pr-2">VAT</td>
                                  <td className="bg-grey-lightest pt-2 pr-2"></td>
                                  <td className="bg-grey-lightest pt-2 pr-2"></td>
                                  <td className="bg-grey-lightest pt-2 pr-2 text-right"><Money amount={ho.totalIncVat - ho.totalExcVat} /></td>
                                </tr>
                                <tr>
                                  <td className="bg-grey-lightest pt-2 pb-2 pl-2 pr-2 font-bold">Total</td>
                                  <td className="bg-grey-lightest pt-2 pb-2 pr-2"></td>
                                  <td className="bg-grey-lightest pt-2 pb-2 pr-2"></td>
                                  <td className="bg-grey-lightest pt-2 pb-2 pr-2 font-bold text-right"><Money amount={ho.totalIncVat} /></td>
                                </tr>
                              </tbody>
                            </table>
                          </td>
                        </tr>
                      ]
                  )) }
                </tbody>
              </table>
            )
          }
        </div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className="bg-past-order-lighter p-2 text-black block no-underline hover:no-underline hover:text-black">
          <h3 className="flex justify-between ml-20"><span>Total:</span><span><Money amount={total} /></span></h3>
        </a>
      </div>
    )
  }
}