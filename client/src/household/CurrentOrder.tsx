import * as React from 'react';
import * as classNames from 'classnames'

import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { ServerApi, ApiError } from '../ServerApi'

export interface CurrentOrderProps { household: Household
                                   , currentOrder: HouseholdOrder | null
                                   , currentCollectiveOrder: CollectiveOrder | null
                                   , products: ProductCatalogueEntry[]
                                   , loading: boolean
                                   , expanded: boolean
                                   , toggle: () => void
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { height: number }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  content: React.RefObject<HTMLDivElement>

  constructor(props: CurrentOrderProps) {
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

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.currentCollectiveOrder)
      return

    const orderId = this.props.currentCollectiveOrder.id
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }

  render() {
    let currentOrder = this.props.currentOrder
    let currentCollectiveOrder = this.props.currentCollectiveOrder

    return (
      <div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} 
           className={classNames(
            'bg-order-dark p-2 block no-underline hover:no-underline text-black hover:text-black', {
              'min-h-0': !this.props.expanded,
              'min-h-20': this.props.expanded 
            })} style={{ 
              transition: this.props.expanded? 'min-height 0.125s 0s ease-in' : 'min-height 0.125s 0.125s ease-out'
            }}>
          <div className="bg-img-order bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Current order
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
        </a>
        <div ref={this.content} style={{
          overflow: 'hidden',
          transition: this.props.expanded? 'height 0.125s 0.125s ease-out' : 'height 0.125s 0s ease-in',
          height: this.props.expanded? this.state.height : 0,
          boxShadow: 'rgba(0, 0, 0, 0.1) 0px 5px 5px 0px inset'
        }}>
          { currentOrder
            ? (
              <CurrentHouseholdOrder householdOrder={currentOrder}
                                     products={this.props.products}
                                     loading={this.props.loading}
                                     reload={this.props.reload}
                                     request={this.props.request} />
            )
            : (currentCollectiveOrder
            ? (
              <div className="p-2 mb-2 mt-2 text-grey-darker">
                <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><strong>{currentCollectiveOrder.createdByName}</strong> started an order on <strong>{Util.formatDate(currentCollectiveOrder.createdDate)}</strong></p  >
                <button className="mt-4" onClick={_ => this.joinOrder()}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
              </div>
            )
            : (
              <div className="p-2 mb-2 mt-2 text-grey-darker">
                <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
                <button className="mt-4" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
              </div>
           ))
          }
        </div>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} className="bg-order-dark p-2 block no-underline hover:no-underline text-black hover:text-black">
          <h3 className="flex justify-between ml-20"><span>Total:</span><span><Money amount={currentOrder? currentOrder.totalIncVat : 0} /></span></h3>
        </a>
      </div>
    )
  }
}