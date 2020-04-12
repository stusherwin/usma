import * as React from 'react';
import * as classNames from 'classnames'

import { ServerApi } from 'util/ServerApi'
import { CollectiveOrder, UploadedOrderFile } from 'util/Types'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'

import { HouseholdOrderItems } from 'household/HouseholdOrderItems'

import { OrderStatus } from 'order/OrderStatus'
import { OrderTotal } from 'order/OrderTotal'

export interface HouseholdOrdersProps { order: CollectiveOrder
                                        request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        reload: () => Promise<void>
                                      }

export interface HouseholdOrdersState { uploadingHouseholdId: number | undefined
                                        uploadedFile: File | undefined
                                        uploadedFileData: UploadedOrderFile | undefined
                                        collapsibleState: CollapsibleState 
                                      }

export class HouseholdOrders extends React.Component<HouseholdOrdersProps, HouseholdOrdersState> {
  constructor(props: HouseholdOrdersProps) {
    super(props)

    this.state = { uploadingHouseholdId: undefined
                 , uploadedFile: undefined
                 , uploadedFileData: undefined
                 , collapsibleState: new CollapsibleState(null, collapsibleState => this.setState({collapsibleState}))
                 }
  }

  startUpload = (householdId: number) => {
    this.setState({ uploadingHouseholdId: householdId
                  , uploadedFile: undefined
                  , uploadedFileData: undefined
                  })
  }

  confirmUpload = () => {
    if(!this.state.uploadedFile || !this.state.uploadingHouseholdId) return

    var formData = new FormData()
    formData.append('files', this.state.uploadedFile, this.state.uploadedFile.name)

    this.props.request(ServerApi.command.uploadOrderFile(formData))
      .then(uploadedFileData => 
        this.setState({ uploadedFileData: uploadedFileData
                      })
      )
  }

  reconcileOrder = () => {
    if(!this.state.uploadedFile || !this.state.uploadingHouseholdId || !this.state.uploadedFileData) return

    this.props.request(ServerApi.command.reconcileHouseholdOrderFromFile(this.props.order.id, this.state.uploadingHouseholdId, this.state.uploadedFileData.fileId))
      .then(() => this.props.reload())
      .then(_ => {
        this.setState({ uploadingHouseholdId: undefined
                      , uploadedFile: undefined
                      , uploadedFileData: undefined
                      })
      })
  }

  cancelUpload = () => {
    this.setState({ uploadingHouseholdId: undefined
                  , uploadedFile: undefined
                  , uploadedFileData: undefined
                  })
  }

  fileChanged = (file: File | undefined) => {
    this.setState({uploadedFile: file})
  }

  render() {
    const order = this.props.order

    return !order.householdOrders.filter(ho => !!ho.items.length).length?
      <div className="px-2 py-4 text-black">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No households added to this order
      </div>
    : <div className="mt-4">
        {order.householdOrders.filter(ho => !!ho.items.length).map(ho => {
          return (
            <div key={ho.householdId}>
              <Collapsible collapsibleKey={ho.householdId}
                           collapsibleState={this.state.collapsibleState}
                           header={
                             <div className="p-2 pt-4 bg-household-lighter min-h-24">
                               <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
                               <div className="flex items-baseline justify-between ml-20">
                                 <div>
                                   <h3 className={classNames("leading-none", {"line-through": ho.isAbandoned})}>
                                     {ho.householdName}
                                   </h3>
                                   <h4 className="mt-4 mb-4">
                                     <OrderStatus order={ho} />
                                   </h4>
                                 </div>
                                 <h4 className="ml-2">
                                   <OrderTotal order={ho} />
                                 </h4>
                               </div>
                             </div>
                           }
                           expandedHeader={
                             order.orderIsPlaced?
                               <div className="p-2 bg-household-lighter -mt-4 flex flex-wrap justify-start content-start items-start">
                                 <button className="flex-no-grow flex-no-shrink mr-2 mt-2" onClick={e => {e.preventDefault(); e.stopPropagation(); this.startUpload(ho.householdId)}} disabled={!!this.state.uploadingHouseholdId}><Icon type="upload" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Upload order file to reconcile</button>
                               </div>
                             : <div></div>
                           }>
                {this.state.uploadingHouseholdId == ho.householdId && 
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
                }
                <div className={classNames("bg-white border-t border-household-light", {"shadow-inner-top": this.state.uploadingHouseholdId != ho.householdId})}>
                  <HouseholdOrderItems householdOrder={ho}
                                       readOnly={true}
                                       {...this.props} />
                </div>
              </Collapsible>
            </div>
          )}
        )}
        <div className="pt-4 pb-4 pl-20 pr-2 font-bold text-black pl-2 flex justify-between">
          <span className="pl-2">Total:</span>
          <span className="font-bold text-right">
            { order.adjustment == null || order.adjustment.oldTotalIncVat == order.totalIncVat?
              <Money className={classNames({'line-through text-black': order.isAbandoned})} amount={order.totalIncVat} />
            : <span className="inline-flex flex-col">
                <Money className="line-through text-black" amount={order.adjustment.oldTotalIncVat} />
                <Money className="text-red" amount={order.totalIncVat} />
              </span>
            }
          </span>
        </div>
    </div>
  }
}