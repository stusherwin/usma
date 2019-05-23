import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { CollapsibleWithHeader } from './CollapsibleWithHeader'

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
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = {
      expanded: null,
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
      <CollapsibleWithHeader className="min-h-20"
                             headerClassName="bg-past-order-lighter min-h-20"
                             headerImageClassName="bg-img-order"
                             headerText="Past orders"
                             headerContent={() => (
                               <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span><span><Money amount={total} /></span></h3>
                             )}
                             {...this.props}>
        { !pastOrders.length
          ? <div className="shadow-inner-top px-2 py-4 bg-white text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div>
          : (
            <div className="shadow-inner-top bg-white">
              <table className="border-collapse w-full">
                <tbody>
                  { pastOrders.map((ho, i) => ([
                      <tr key={ho.orderId}>
                        <td className={classNames('pl-2 pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0, 'pt-4': i == 0})}>
                          <a href="#" onClick={e => {e.preventDefault(); this.expandOrder(ho)}}>{Util.formatDate(ho.orderCreatedDate)}</a>
                          <Icon type={this.state.expanded == ho? 'collapse' : 'expand'} className="w-3 h-3 ml-2 text-grey-dark fill-current" />
                        </td>
                        {/* <td className={classNames('pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}>{itemCount(ho)}</td> */}
                        <td className={classNames('pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0, 'pt-4': i == 0})}>{ho.isAbandoned && 'Abandoned'}</td>
                        <td className={classNames('pr-2 text-right', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0, 'pt-4': i == 0, 'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></td>
                      </tr>
                      ,
                      this.state.expanded == ho &&
                        <tr>
                          <td colSpan={3}>
                            <table className="border-collapse w-full bg-grey-lighter shadow-inner-top">
                              <tbody>
                                {ho.items.map(i =>
                                  <tr key={i.productId}>  
                                    <td className="pt-2 pl-2 pr-2">{i.productCode}</td>
                                    <td className="pt-2 pr-2 w-full">{i.productName}</td>
                                    <td className="pt-2 pr-2 whitespace-no-wrap">x {i.itemQuantity}</td>
                                    <td className={classNames('pt-2 pr-2 text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={i.itemTotalExcVat} /></td>
                                  </tr>
                                )}
                                <tr>
                                  <td className="pt-2 pr-2 pl-2">VAT:</td>
                                  <td className="pt-2 pr-2"></td>
                                  <td className="pt-2 pr-2"></td>
                                  <td className={classNames('pt-2 pr-2 text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat - ho.totalExcVat} /></td>
                                </tr>
                                <tr>
                                  <td className="pt-4 pb-2 pl-2 pr-2 font-bold">Total:</td>
                                  <td className="pt-4 pb-2 pr-2"></td>
                                  <td className="pt-4 pb-2 pr-2"></td>
                                  <td className={classNames('pt-4 pr-2 pb-2 font-bold text-right', {'line-through text-grey-dark': ho.isAbandoned})}><Money amount={ho.totalIncVat} /></td>
                                </tr>
                              </tbody>
                            </table>
                          </td>
                        </tr>
                      ]
                  )) }
                  <tr>
                    <td className="pt-4 pl-2 pr-2 pb-4 font-bold">Total:</td>
                    {/* <td className="pt-2 pb-2 pr-2"></td> */}
                    <td className="pt-4 pb-2 pr-2 pb-4"></td>
                    <td className="pt-4 pr-2 pb-4 font-bold text-right"><Money amount={total} /></td>
                  </tr>
                </tbody>
              </table>
            </div>
          )
        }
      </CollapsibleWithHeader>
    )
  }
}