import * as React from 'react'
import * as classNames from 'classnames'

import { CollectiveOrder, Household, GroupSettings, UploadedOrderFile } from 'util/Types'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'

import { OrderTabs } from 'order/OrderTabs'
import { OrderItems } from 'order/OrderItems'
import { OrderTotal } from 'order/OrderTotal'
import { OrderStatus } from 'order/OrderStatus'
import { OrderMessages, getMessages } from 'order/OrderMessages'

import { AdminTopNav } from 'admin/AdminTopNav'

import { PastCollectiveOrders } from './PastCollectiveOrders'
import { HouseholdOrders } from './HouseholdOrders'
import { ProductCodes } from './ProductCodes'
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { ReconcileOrder } from './ReconcileOrder'

export interface AdminOrdersPageProps { collectiveOrder: CollectiveOrder | undefined
                                        pastOrders: CollectiveOrder[]
                                        households: Household[]
                                        groupSettings: GroupSettings
                                        request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        reload: () => Promise<void>
                                      }

export interface AdminOrdersPageState { collapsibleState: CollapsibleState 
                                        addingHousehold: Household | undefined
                                        tab: 'households' | 'product-list' | 'product-codes'
                                        reconcilingOrder: boolean
                                        uploading: boolean
                                        uploadedFile: File | undefined
                                        uploadedFileData: UploadedOrderFile | undefined
                                        selectedHouseholdId: number | undefined
                                      }

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {  
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = { 
      collapsibleState: new CollapsibleState('order', collapsibleState => this.setState({collapsibleState})),
      addingHousehold: undefined,
      tab: 'households',
      reconcilingOrder: false,
      uploading: false,
      uploadedFile: undefined,
      uploadedFileData: undefined,
      selectedHouseholdId: undefined
    }
  }

  newOrder = () => {
    this.props.request(ServerApi.command.createOrder())
      .then(this.props.reload)
  }

  abandonOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  placeOrder = () => {
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.placeOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  startReconcilingOrder = () => {
    if(!this.props.collectiveOrder) return

    this.setState({reconcilingOrder: true})
  }

  endReconcilingOrder = () => {
    if(!this.state.reconcilingOrder) return
    
    this.setState({reconcilingOrder: false})
  }

  endReconcilingItem = (productId: number, productPriceExcVat: number, households: {householdId: number, itemQuantity: number}[]) => {
    if(!this.state.reconcilingOrder) return
    if(!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.reconcileOrderItem(this.props.collectiveOrder.id, productId, productPriceExcVat, households))
      .then(this.props.reload)
  }

  startUpload = () => {
    if(!this.props.collectiveOrder) return

    this.setState({ uploading: true
                  , uploadedFile: undefined
                  , uploadedFileData: undefined
                  })
  }

  fileChanged = (file: File | undefined) => {
    if(!this.props.collectiveOrder) return

    this.setState({uploadedFile: file})
  }

  confirmUpload = () => {
    if(!this.props.collectiveOrder) return
    if(!this.state.uploadedFile || !this.state.uploading) return
    let selectedHouseholdId = (this.props.collectiveOrder.householdOrders[0] || {}).householdId

    var formData = new FormData()
    formData.append('files', this.state.uploadedFile, this.state.uploadedFile.name)

    this.props.request(ServerApi.command.uploadOrderFile(formData))
      .then(uploadedFileData => 
        this.setState({ uploadedFileData: uploadedFileData
                      , selectedHouseholdId
                      })
      )
  }

  reconcileOrder = () => {
    if(!this.props.collectiveOrder) return
    if(!this.state.uploadedFile || !this.state.uploading || !this.state.uploadedFileData || !this.state.selectedHouseholdId) return

    this.props.request(ServerApi.command.reconcileHouseholdOrderFromFile(this.props.collectiveOrder.id, this.state.selectedHouseholdId, this.state.uploadedFileData.fileId))
      .then(() => this.props.reload())
      .then(_ => {
        this.setState({ uploading: false
                      , uploadedFile: undefined
                      , uploadedFileData: undefined
                      })
      })
  }

  cancelUpload = () => {
    if(!this.props.collectiveOrder) return

    this.setState({ uploading: false
                  , uploadedFile: undefined
                  , uploadedFileData: undefined
                  })
  }

  render() {
    const order = this.props.collectiveOrder
    const messages = getMessages(order)

    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <Collapsible collapsibleKey="order"
                     collapsibleState={this.state.collapsibleState}
                     onCollapsed={this.endReconcilingOrder}
                     {...this.props}
                     header={
                       <div className="p-2 pt-4 bg-order-dark min-h-24">
                         <div className="bg-no-repeat w-16 h-16 absolute bg-img-order"></div>
                         <div className="flex items-baseline justify-between ml-20">
                           <div>
                             <h2 className="leading-none">
                               Current order
                             </h2>
                             <h3 className="mt-4">
                               <OrderStatus order={order} />
                             </h3>
                           </div>
                           <h3 className="ml-2">
                             <OrderTotal order={order} />
                           </h3>
                         </div>
                       </div>
                     }
                     expandedHeader={
                       <div className="p-2 bg-order-dark -mt-4">
                         {!this.state.reconcilingOrder && !this.state.uploading &&
                           <CollectiveOrderButtons order={order}
                                                   newOrder={this.newOrder} 
                                                   abandonOrder={this.abandonOrder} 
                                                   placeOrder={this.placeOrder}
                                                   reconcileOrder={this.startReconcilingOrder}
                                                   uploadOrderFile={this.startUpload} />
                         }
                         {!this.state.reconcilingOrder && !this.state.uploading && !!order && !!order.items.length &&
                           <div className="mt-5">
                             <OrderTabs tab={this.state.tab} 
                                        setTab={tab => this.setState({tab})}
                                        householdsBg={messages.length? 'bg-blue-lighter' : 'bg-household-lightest'}
                                        productsBg={messages.length? 'bg-blue-lighter' : 'bg-white'}
                                        productCodesBg={messages.length? 'bg-blue-lighter' : 'bg-white'} />
                           </div>
                         }
                       </div>
                     }>
          { order && (
            this.state.reconcilingOrder?
              <ReconcileOrder order={order} 
                              endReconcilingOrder={this.endReconcilingOrder}
                              endReconcilingItem={this.endReconcilingItem} />
            : !order.items.length?
              <div className="shadow-inner-top border-t bg-white px-2 py-4 text-black">
                <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
              </div>
            : this.state.uploading?
              <div className="bg-household-lightest px-2 py-4 shadow-inner-top">
                {this.state.uploadedFileData === null &&
                  <div className="p-2 bg-red-lighter -mx-2 mb-4 -mt-4 shadow-inner-top flex">
                    <Icon type="error" className="flex-no-shrink w-4 h-4 mr-2 fill-current nudge-d-1" />
                    <span>Could not read the order file: either the wrong file was uploaded or the file layout has changed.</span>
                  </div>
                }
                <h3 className="mb-4">Upload file to reconcile order items</h3>
                {!this.state.uploadedFileData &&
                  <div className="field mb-4">
                    <div className="flex justify-between items-baseline">
                      <label className="flex-no-grow flex-no-shrink mr-2"
                             htmlFor="upload">Choose file: </label>
                      <input type="file"
                             name="file"
                             id="upload"
                             className="flex-grow flex-no-shrink"
                             onChange={e => this.fileChanged((e.target.files || [])[0])} />
                    </div>
                  </div>
                }
                {!!this.state.uploadedFileData &&
                  <div className="bg-white -mx-2 p-2 pt-4 mb-4">
                    <div>Order reference:<br/>{this.state.uploadedFileData.orderDescription}</div>
                    <table className="mt-3 mb-1">
                      {this.state.uploadedFileData.rows.map(r =>
                        <tr>
                          <td className="align-baseline pr-2 pb-1 pt-1">{r.code}: {r.productDescription} ({r.productSize})</td>
                          <td className="align-baseline pr-2 pb-1 pt-1 whitespace-no-wrap text-right"><Money amount={r.price} /></td>
                          <td className="align-baseline pr-2 pb-1 pt-1 whitespace-no-wrap">x {r.quantity}</td>
                          <td className="align-baseline pb-1 pt-1 whitespace-no-wrap text-right"><Money amount={r.total} /></td>
                        </tr> 
                      )}
                      <tr>
                        <td className="align-baseline pr-2 pb-1 pt-1 whitespace-no-wrap text-right" colSpan={3}>Goods Total:</td>
                        <td className="align-baseline pb-1 pt-1 whitespace-no-wrap text-right"><Money amount={this.state.uploadedFileData.totalExcVat} /></td>
                      </tr>
                      <tr>
                        <td className="align-baseline pr-2 pb-1 pt-1 whitespace-no-wrap text-right" colSpan={3}>Vat:</td>
                        <td className="align-baseline pb-1 pt-1 whitespace-no-wrap text-right"><Money amount={this.state.uploadedFileData.totalIncVat - this.state.uploadedFileData.totalExcVat} /></td>
                      </tr>
                      <tr>
                        <td className="align-baseline pr-2 pb-1 pt-1 whitespace-no-wrap text-right" colSpan={3}>Net Due:</td>
                        <td className="align-baseline pb-1 pt-1 whitespace-no-wrap text-right"><Money amount={this.state.uploadedFileData.totalIncVat} /></td>
                      </tr>
                    </table>
                  </div>
                }
                {!!this.state.uploadedFileData &&
                  <div className="mb-4">
                    <div className="mb-2">Select a household to apply the order to:</div>
                    <div>
                      <select className="border" value={this.state.selectedHouseholdId} onChange={e => this.setState({selectedHouseholdId: parseInt(e.target.value)})}>
                        {order.householdOrders.map(ho => <option key={ho.householdId} value={ho.householdId}>{ho.householdName}</option>)}
                      </select>
                    </div>
                  </div>
                }
                <div className="flex justify-end">
                  {!this.state.uploadedFileData &&
                    <button className="ml-2" onClick={this.confirmUpload} disabled={!this.state.uploadedFile}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Upload</button>
                  }
                  {!!this.state.uploadedFileData &&
                    <button className="ml-2" onClick={this.reconcileOrder} disabled={!this.state.uploadedFile}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Reconcile order</button>
                  }
                  <button className="ml-2" onClick={this.cancelUpload}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
                </div>
              </div>
            : this.state.tab == 'households'?
              <div className={classNames("border-t bg-household-lightest", {"shadow-inner-top": !this.state.uploading})}>
                <OrderMessages order={order} />
                <div className="flex justify-end mt-4 mr-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url("query/household-orders-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                </div>
                <HouseholdOrders order={order}
                                 {...this.props} />
              </div>
            : this.state.tab == 'product-list'?
              <div className={classNames("border-t bg-white", {"shadow-inner-top": !this.state.uploading})}>
                <OrderMessages order={order} />
                <div className="flex justify-end mr-2 mt-4 mb-2">
                  <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                </div>
                <OrderItems order={order} />
              </div>
            : <div className={classNames("border-t border-t bg-white", {"shadow-inner-top": !this.state.uploading})}>
                <OrderMessages order={order} />
                <ProductCodes order={order} />
              </div>
            )
          }
        </Collapsible>
        <PastCollectiveOrders collapsibleKey="past-orders"
                              collapsibleState={this.state.collapsibleState}
                              {...this.props} />
      </div>
    )
  }
}