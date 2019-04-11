import * as React from 'react';
import * as classNames from 'classnames'

import { PastHouseholdOrder, HouseholdOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'

export interface PastHouseholdOrdersProps { householdOrders: PastHouseholdOrder[]
                                          , basePath: string
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          }

export class PastHouseholdOrders extends React.Component<PastHouseholdOrdersProps, {expanded: PastHouseholdOrder | null}> {
  constructor(props: PastHouseholdOrdersProps) {
    super(props)

    this.state = {
      expanded: null
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
      <div>
        <RouterLink path={this.props.basePath} className="bg-grey-lighter p-2 block no-underline hover:no-underline text-grey-darkest hover:text-grey-darkest">
          <div className="bg-img-order-bw bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2">
            <h2 className="text-grey-darkest leading-none mb-2 -mt-1">Past orders</h2>
            <h3 className="mt-0 flex justify-between"><span>Total orders:</span><span><Money amount={total} /></span></h3>
          </div>
        </RouterLink>
        {!pastOrders.length
        ? <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders</div>
        : (
          <table className="border-collapse w-full mb-4">
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
              {!!pastOrders.length &&
                <tr>
                  <td className="pt-8 pl-2 pr-2 font-bold">Total orders</td>
                  <td className="pt-8 pr-2"></td>
                  <td className="pt-8 pr-2"></td>
                  <td className="pt-8 pr-2 text-right font-bold"><Money amount={total} /></td>
                </tr>
              }
            </tbody>
          </table>
        )}
      </div>
    )
  }
}