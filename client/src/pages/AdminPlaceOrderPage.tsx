import * as React from 'react';

import { CollectiveOrder } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { Money } from '../common/Money'
import { AdminTopNav } from '../components/AdminTopNav'
import { Icon } from '../common/Icon'
import { Router } from '../common/Router'

export interface AdminPlaceOrderPageProps { currentOrder: CollectiveOrder
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
  
    return (
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-order-dark p-2">
          <AdminTopNav />
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="leading-none mb-2 -mt-1">Place order{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <div>
              <h3 className="mt-0 flex justify-between"><span>{Util.formatDate(currentOrder.createdDate)}</span><span><Money amount={currentOrder.totalIncVat} /></span></h3>
              <h3 className="font-normal">{currentOrder.status}</h3>
            </div>
          </div>
          <div>
            <button className="mr-2 mt-2" onClick={this.placeOrder}><Icon type="ok" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Confirm</button>
            <button className="mr-2 mt-2" onClick={this.cancel}><Icon type="cancel" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Cancel</button>
          </div>
        </div>
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
          <div className="">
            {this.props.currentOrder.items.map(i => (
              <div key={i.productId} className="p-2 mb-4 mt-4">
                <div className="flex justify-between items-baseline">
                  <span className="flex-no-shrink flex-no-grow font-bold w-1/3">{i.productCode}</span>
                  <span className="flex-no-shrink flex-no-grow w-1/3 text-center">
                    <span>x {i.itemQuantity}</span>
                  </span>
                  <Money className="flex-no-shrink flex-no-grow w-1/3 text-right" amount={i.itemTotalExcVat} />
                </div>
                <div className="flex justify-between items-end mt-2">
                  <span className="flex-no-grow">{i.productName}</span>
                </div>
              </div>
            ))}
            <div className="p-2 flex justify-between items-baseline">
              <span className="flex-no-shrink flex-no-grow font-bold">Total</span>
              <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={currentOrder.totalIncVat} />
            </div>
          </div>
        }
        {this.state.view == "codes" && 
          <div className="mb-4 mt-4 p-2">
            <textarea className="border p-2 w-full leading-tight" style={{minHeight: `${currentOrder.items.length * 1.25 + 1.5}rem`}}>
              {currentOrder.items.map(i => `${i.productCode} ${i.itemQuantity}`).join('\n')}
            </textarea>
          </div>
        }
      </div>
    )
  }
}