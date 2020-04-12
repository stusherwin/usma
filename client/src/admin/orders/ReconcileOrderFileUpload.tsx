import * as React from 'react'
import * as classNames from 'classnames'

import { CollectiveOrder, UploadedOrderFile } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { Money } from 'util/Money'

export interface ReconcileOrderFileUploadProps { collectiveOrder: CollectiveOrder
                                                 bgColor: string
                                                 cancelUpload: () => void
                                                 request: <T extends {}>(p: Promise<T>) => Promise<T>
                                                 reload: () => Promise<void>
                                               }

export interface ReconcileOrderFileUploadState { uploadedFile: File | undefined
                                                 uploadedFileData: UploadedOrderFile | undefined
                                                 selectedHouseholdId: number | undefined
                                               }

export class ReconcileOrderFileUpload extends React.Component<ReconcileOrderFileUploadProps, ReconcileOrderFileUploadState> {  
  constructor(props: ReconcileOrderFileUploadProps) {
    super(props)

    this.state = { 
      uploadedFile: undefined,
      uploadedFileData: undefined,
      selectedHouseholdId: undefined
    }
  }

  fileChanged = (file: File | undefined) => {
    this.setState({uploadedFile: file})
  }

  confirmUpload = () => {
    if(!this.state.uploadedFile) return

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
    if(!this.state.uploadedFile || !this.state.uploadedFileData || !this.state.selectedHouseholdId) return

    this.props.request(ServerApi.command.reconcileHouseholdOrderFromFile(this.props.collectiveOrder.id, this.state.selectedHouseholdId, this.state.uploadedFileData.fileId))
      .then(() => this.props.reload())
      .then(_ => {
    
        this.props.cancelUpload()
        this.setState({ uploadedFile: undefined
                      , uploadedFileData: undefined
                      })
      })
  }

  cancelUpload = () => {
    this.props.cancelUpload()
    this.setState({ uploadedFile: undefined
                  , uploadedFileData: undefined
                  })
  }

  render() {
    const order = this.props.collectiveOrder

    return (
      <div className={classNames(this.props.bgColor, "px-2 py-4 shadow-inner-top")}>
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
    )
  }
}