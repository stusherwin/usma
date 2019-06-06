import * as React from 'react';
import * as classNames from 'classnames'

import { PastCollectiveOrder } from '../../Types'
import { ApiError } from '../../ServerApi'
import { Util } from '../../common/Util'
import { RouterLink } from '../../common/RouterLink'
import { Icon } from '../../common/Icon'
import { Money } from '../../common/Money'
import { TopNav } from '../TopNav'
import { CollapsibleWithHeader } from '../../household/CollapsibleWithHeader'
import { Loading } from '../../household/Loading'

export interface PastOrdersPageProps { pastOrders: PastCollectiveOrder[]
                                     , loading: boolean
                                     , error: ApiError | null
                                     }
type Section = 'past-orders'
export interface PastOrdersPageState { expanded: Section | null
                                     }

export class PastOrdersPage extends React.Component<PastOrdersPageProps, PastOrdersPageState> {  
  constructor(props: PastOrdersPageProps) {
    super(props)

    this.state = { 
      expanded: 'past-orders', 
    }
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  render() {
    const pastOrders = this.props.pastOrders

    return (
      <div className="bg-past-order-lighter min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <TopNav />
        <CollapsibleWithHeader className="min-h-20"
                               headerClassName="bg-past-order-lighter min-h-20"
                               headerImageClassName="bg-img-order"
                               headerText="Past orders"
                               expanded={this.state.expanded == 'past-orders'}
                               otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders'}
                               toggle={this.toggle('past-orders')}>
          <div className="bg-white shadow-inner-top">
            {!pastOrders.length
            ? <div className="px-2 py-4 text-grey-darker">
                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No past orders
              </div> 
            : (
              <table className="border-collapse w-full">
                <tbody>
                  { pastOrders.map((o, i) => (
                    <tr key={o.id}>
                      <td className={classNames("pt-4 pl-2 pr-2", {"pb-4": i == pastOrders.length -1})}><RouterLink path={`/admin/orders/${o.id}`}>{ Util.formatDate(o.createdDate)}</RouterLink></td>
                      <td className={classNames("pt-4 pr-2", {"pb-4": i == pastOrders.length -1})}>{o.isAbandoned && 'Abandoned'}</td>
                      <td className={classNames("pt-4 pr-2 text-right", {"pb-4": i == pastOrders.length -1, "line-through text-grey-dark": o.isAbandoned})}><Money amount={o.totalIncVat} /></td>
                    </tr>
                  )) }
                </tbody>
              </table>
            )}
          </div>
        </CollapsibleWithHeader>
        <Loading loading={this.props.loading}></Loading>
      </div>
    )
  }
}