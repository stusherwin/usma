import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder, PastHouseholdOrder, PastOrderItem } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Collapsible } from '../common/Collapsible'
import { PastHouseholdOrders } from './PastHouseholdOrdersForOrder';

export interface PastOrdersProps { pastOrders: PastCollectiveOrder[]
                                 , pastHouseholdOrders: PastHouseholdOrder[]
                                 , expanded: boolean
                                 , otherExpanding: boolean
                                 , toggle: () => void
                                 }
export interface PastOrdersState { expanded: PastCollectiveOrder | null }

export class PastOrders extends React.Component<PastOrdersProps, PastOrdersState> {  
  constructor(props: PastOrdersProps) {
    super(props)

    this.state = { 
      expanded: null, 
    }
  }

  toggle = (toExpand: PastCollectiveOrder) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    const pastOrders = this.props.pastOrders

    return (
      <Collapsible className="min-h-20"
                   {...this.props}
                   header={() => 
                     <div className="p-2 bg-past-order-lighter min-h-20">
                       <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                       <h2 className="leading-none ml-20 relative flex">
                         Past orders
                       </h2>
                     </div>
                   }>
        <div className="bg-white shadow-inner-top">
          {!pastOrders.length
          ? <div className="px-2 py-4 text-grey-darker">
              <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
            </div> 
          : (
            <table className="border-collapse w-full">
              <tbody>
                { pastOrders.map((o, i) => ([
                  <tr key={o.id}>
                    <td className={classNames('pl-2 pr-2 pt-4', {'pb-2': this.state.expanded == o, 'pb-4': i == pastOrders.length - 1 && this.state.expanded != o})}>
                      <a href="#" onClick={e => {e.preventDefault(); this.toggle(o)()}}>{Util.formatDate(o.createdDate)}</a>
                      <Icon type={this.state.expanded == o? 'collapse' : 'expand'} className="w-3 h-3 ml-2 text-grey-dark fill-current" />
                    </td>
                    {/* <td className={classNames('pr-2', {'pb-2': this.state.expanded == ho, 'pt-2': i > 0})}>{itemCount(ho)}</td> */}
                    <td className={classNames('pr-2 pt-4', {'pb-2': this.state.expanded == o, 'pb-4': i == pastOrders.length - 1 && this.state.expanded != o})}>{o.isAbandoned && 'Abandoned'}</td>
                    <td className={classNames('pr-2 pt-4 text-right', {'pb-2': this.state.expanded == o, 'pb-4': i == pastOrders.length - 1 && this.state.expanded != o, 'line-through text-grey-dark': o.isAbandoned})}><Money amount={o.totalIncVat} /></td>
                  </tr>
                  ,
                  this.state.expanded == o &&
                    <tr>
                      <td colSpan={3}>
                        <PastHouseholdOrders pastOrder={o}
                                             pastHouseholdOrders={this.props.pastHouseholdOrders.filter(ho => ho.orderId == o.id)} />
                      </td>
                    </tr>
                  ]
                )) }
              </tbody>
            </table>
          )}
        </div>
      </Collapsible>
    )
  } 
}