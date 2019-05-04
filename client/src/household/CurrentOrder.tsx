import * as React from 'react';
import * as classNames from 'classnames'

import { CurrentHouseholdOrder } from './CurrentHouseholdOrder'
import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Util } from '../common/Util'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { ServerApi, ApiError } from '../ServerApi'

const transitionTime = 0.25;
const minHeight = '5rem';

export interface CurrentOrderProps { household: Household
                                   , currentOrder: CollectiveOrder | null
                                   , currentHouseholdOrders: HouseholdOrder[]
                                   , currentHouseholdOrder: HouseholdOrder | null
                                   , products: ProductCatalogueEntry[]
                                   , households: Household[]
                                   , loading: boolean
                                   , expanded: boolean
                                   , otherExpanding: boolean
                                   , toggle: () => void
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   }

export interface CurrentOrderState { addingProduct: boolean
                                   }

export class CurrentOrder extends React.Component<CurrentOrderProps, CurrentOrderState> {
  container: React.RefObject<HTMLDivElement>

  constructor(props: CurrentOrderProps) {
    super(props)

    this.container = React.createRef();
    this.state = { addingProduct: false
               }
  }

  componentDidUpdate(prevProps: CurrentOrderProps) {
    if(prevProps.expanded != this.props.expanded) {
      this.animateHeight()
      if(!prevProps.expanded) {
        this.setState({addingProduct: false})
      }
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

  startAdd = () => this.setState({ addingProduct: true })

  cancelAdd = () => this.setState({ addingProduct: false })

  confirmAdd = (product: ProductCatalogueEntry) => {
    if(!this.state.addingProduct) return Promise.resolve();
    if(!this.props.currentHouseholdOrder) return Promise.resolve();

    return this.props.request(ServerApi.command.ensureHouseholdOrderItem(this.props.currentHouseholdOrder.orderId, this.props.currentHouseholdOrder.householdId, product.code, 1))
      .then(this.props.reload)
  }

  newOrder = () => {
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createOrder(householdId))
      .then(this.props.reload)
  }

  joinOrder = () => {
    if(!this.props.currentOrder)
      return

    const orderId = this.props.currentOrder.id
    const householdId = this.props.household.id
    this.props.request(ServerApi.command.createHouseholdOrder(orderId, householdId))
      .then(this.props.reload)
  }

  render() {
    let householdOrder = this.props.currentHouseholdOrder
    let order = this.props.currentOrder

    return (
      <div ref={this.container} className="min-h-20" style={{ 
          overflow: 'hidden',
          height: minHeight,
          transition: `height ${transitionTime / 2}s ease`,
          transitionDelay: this.props.expanded? '0s' : (this.props.otherExpanding? `${transitionTime / 2}s` : '0s')
        }} onTransitionEnd={this.unsetHeight}>
        <a href="#" onClick={e => { e.preventDefault(); this.props.toggle() }} 
          className="bg-order-dark p-2 block no-underline hover:no-underline text-black hover:text-black min-h-20">
          <div className="bg-img-order bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 relative flex">Current order
            <Icon type={this.props.expanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
          </h2>
          <h3 className="flex justify-between ml-20 mt-4"><span>Total:</span>
            <span>
              {householdOrder 
              ? householdOrder.newTotalIncVat !== null && !householdOrder.isAbandoned
                ? <span>
                    <span className="line-through"><Money amount={householdOrder.totalIncVat} /></span> 
                    <Money className="text-red font-bold" amount={householdOrder.newTotalIncVat} />
                  </span>
                  : <Money amount={!householdOrder.isAbandoned? householdOrder.totalIncVat : 0} />
              : <Money amount={0} />
              }
            </span>
          </h3>
        </a>
        { householdOrder && order
          ? (
            <CurrentHouseholdOrder currentOrder={order}
                                   currentHouseholdOrder={householdOrder}
                                   currentHouseholdOrders={this.props.currentHouseholdOrders}
                                   products={this.props.products}
                                   households={this.props.households}
                                   loading={this.props.loading}
                                   reload={this.props.reload}
                                   request={this.props.request}
                                   addingProduct={this.state.addingProduct}
                                   startAdd={this.startAdd}
                                   cancelAdd={this.cancelAdd}
                                   confirmAdd={this.confirmAdd} />
          )
          : (order
          ? (
            <div className="shadow-inner-top bg-white px-2 py-4 text-grey-darker">
              <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" /><strong>{order.createdByName}</strong> started an order on <strong>{Util.formatDate(order.createdDate)}</strong></p  >
              <button className="mt-4" onClick={_ => this.joinOrder()}><Icon type="enter" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Join this order</button>
            </div>
          )
          : (
            <div className="shadow-inner-top bg-white px-2 py-4 text-grey-darker">
              <p><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />There's no order currently in progress.</p>
              <button className="mt-4" onClick={this.newOrder}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Start a new one</button>
            </div>
         ))
        }
      </div>
    )
  }
}