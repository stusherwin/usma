import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder, HouseholdOrder, OrderItem } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Money } from '../common/Money'
import { AdminTopNav } from '../components/AdminTopNav'
import { Icon } from '../common/Icon'
import { Router } from '../common/Router'

export interface AdminPlaceOrderPageProps { currentOrder: CollectiveOrder
                                          , currentHouseholdOrders: HouseholdOrder[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          , loading: boolean
                                          , error: ApiError | null
                                          }

export interface AdminPlaceOrderPageState { view: "summary" | "codes"
                                     }
                                     
export class AdminPlaceOrderPage extends React.Component<AdminPlaceOrderPageProps, AdminPlaceOrderPageState> {
  constructor(props: AdminPlaceOrderPageProps) {
    super(props)

    this.state = { view: "summary"
                 }
  }

  placeOrder = () => {
    this.props.request(ServerApi.command.placeOrder(this.props.currentOrder.id))
      .then(this.props.reload)
      .then(_ => Router.navigate(`/admin/orders`))
  }

  cancel = () => {
    Router.navigate('/admin/orders')
  }

  render() {
    const currentOrder = this.props.currentOrder
    const householdOrders = this.props.currentHouseholdOrders
    const allComplete = householdOrders.reduce((complete, ho) => complete && !ho.isOpen, true)
    const orderMinimumReached = currentOrder && currentOrder.totalIncVat >= 25000
    const placeOrderAllowed = allComplete /*&& allPaid*/ && orderMinimumReached
  
    return (
      <div className="bg-order-dark min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <AdminTopNav />
        <div className="p-2 bg-order-dark min-h-20">
          <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
          <h2 className="leading-none ml-20 relative flex">
            Place order
          </h2>
          <h3 className="flex justify-between ml-20 mt-4 mb-2">
            {this.renderStatus()}
            <span className="flex justify-end">
              <span>Total:</span>
              <span className="w-24 font-bold text-right">{this.renderTotal()}</span>
            </span>
          </h3>
          <div>
            <button className="mr-2 mt-2" disabled={!placeOrderAllowed} onClick={this.placeOrder}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Confirm</button>
            <button className="mr-2 mt-2" onClick={this.cancel}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Cancel</button>
          </div>
        </div>
        <div className="shadow-inner-top bg-white px-2 py-4">
          <div className="p-2 flex justify-end">
            <div>
              {this.state.view == "summary"?
                <span>Order summary</span> 
              : <a href="#" onClick={e => {e.preventDefault(); this.setState({view: "summary"})}}>Order summary</a>
              }
              &nbsp;|&nbsp;
              {this.state.view == "codes"?
              <span>Product codes</span>
              : <a href="#" onClick={e => {e.preventDefault(); this.setState({view: "codes"})}}>Product codes</a>
              }
            </div>
          </div>
          {this.state.view == "summary" && 
            <table className="border-collapse w-full">
              {this.props.currentOrder.items.map(this.renderItem)}
              <tr>
                <td className={classNames('pt-8 align-baseline px-2')} colSpan={5}>
                  <div className="flex justify-end">
                    <span>VAT:</span>
                    <span className={classNames('w-24 text-right')}>
                      <span><Money amount={currentOrder.totalIncVat - currentOrder.totalExcVat} /></span>
                    </span>
                  </div>
                </td>
              </tr>
              <td className={classNames('pt-8 align-baseline px-2')} colSpan={5}>
                <div className="flex justify-end font-bold">
                  <span>Total</span>
                  <span className="w-24 text-right">
                    <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={currentOrder.totalIncVat} />
                  </span>
                </div>
              </td>
            </table>
          }
          {this.state.view == "codes" && 
            <div>
              <textarea className="border p-2 w-full leading-tight" style={{minHeight: `${currentOrder.items.length * 1.25 + 1.5}rem`}}>
                {currentOrder.items.map(i => `${i.productCode} ${i.itemQuantity}`).join('\n')}
              </textarea>
            </div>
          }
        </div>
      </div>
    )
  }

  renderStatus = () => {
    return (
      <span>
        {!this.props.currentOrder?
          <span><Icon type="info" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Available</span>
        : this.props.currentOrder.isComplete?
          <span><Icon type="ok" className="w-4 h-4 fill-current nudge-d-1 mr-2" />Complete</span>
        : <span><Icon type="play" className="w-4 h-4 fill-current nudge-d-1 mr-2" />In progress</span>
        }
      </span>
    )
  }
  
  renderTotal = () => {
    const order = this.props.currentOrder
    return !order?
      <Money amount={0} />
    : order.oldTotalIncVat === null || order.oldTotalIncVat == order.totalIncVat?
      <Money amount={order.totalIncVat} />
    : <span>
        <span className="line-through"><Money amount={order.oldTotalIncVat} /></span> 
        <Money className="text-red font-bold" amount={order.totalIncVat} />
      </span>
  }
  
  renderItem = (i: OrderItem, ix: number) => 
    [
    <tr key={i.productId + '-1'}>
      <td className={classNames('w-20 h-20 align-top pl-2', {'pt-4': ix == 0, 'pt-8': ix > 0})} rowSpan={3}>
        <img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${i.productCode}`)} />
      </td>
      <td className={classNames('pb-2 pl-2 font-bold align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>{i.productCode}</td>
      <td className={classNames('pl-2 pb-2 align-baseline', {'pt-4': ix == 0, 'pt-8': ix > 0})}>
        <span>x {i.itemQuantity}</span>
      </td>
      <td className={classNames('pl-2 pr-2 pb-2 text-right align-baseline whitespace-no-wrap', {'pt-4': ix == 0, 'pt-8': ix > 0})} colSpan={2}>
        <span><Money amount={i.itemTotalExcVat} /></span>
      </td>
    </tr>
    ,
    <tr key={i.productId + '-2'}>
      <td className={classNames('pb-2 pl-2 align-top')} colSpan={3}>
        {i.productName}
      </td>
      <td className={classNames('pl-2 pr-2 align-top text-right')}>
      </td>
    </tr>
    ,
    <tr key={i.productId + '-3'}>
      <td className={classNames('pl-2 text-grey')} colSpan={3}>VAT: {i.productVatRate} rate</td>
      <td className={classNames('pl-2 pr-2')}>&nbsp;</td>
    </tr>
    ]
}