import * as React from 'react';

import { CollectiveOrder } from '../Types'
import { Icon } from '../common/Icon'

export interface CurrentOrderProductCodesProps { currentOrder: CollectiveOrder
                                               }

export interface CurrentOrderProductCodesState {}
                                     
export class CurrentOrderProductCodes extends React.Component<CurrentOrderProductCodesProps, CurrentOrderProductCodesState> {
  textArea: React.RefObject<HTMLTextAreaElement>

  constructor(props: CurrentOrderProductCodesProps) {
    super(props)

    this.textArea = React.createRef();
  }

  render() {
    const order = this.props.currentOrder
  
    return !order.items.length?
      <div className="px-2 py-4 text-grey-darker">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet
      </div>
    : 
      <div className="px-2 py-4">
        <div className="flex justify-end mb-4">
          <button onClick={e => { if(this.textArea.current) { this.textArea.current.focus(); this.textArea.current.select(); document.execCommand('copy'); } } }><Icon type="copy" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Copy to clipboard</button>
        </div>
        <textarea ref={this.textArea} readOnly className="border p-2 w-full leading-tight bg-grey-lighter shadow-inner-top" style={{minHeight: `${order.items.length * 1.25 + 1.5}rem`}}>
          {order.items.map(i => `${i.productCode} ${i.itemQuantity}`).join('\n')}
        </textarea>
      </div>
  }
}