import * as React from 'react';

import { OrderItem } from '../../util/Types'
import { Icon } from '../../util/Icon'

export interface CollectiveOrderProductCodesProps { items: OrderItem[]
                                                  }

export class CollectiveOrderProductCodes extends React.Component<CollectiveOrderProductCodesProps, {}> {
  textArea: React.RefObject<HTMLTextAreaElement>

  constructor(props: CollectiveOrderProductCodesProps) {
    super(props)

    this.textArea = React.createRef();
  }

  render() {
    const items = this.props.items
  
    return !items.length?
      <div className="px-2 py-4 text-grey-darker">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
      </div>
    : 
      <div className="px-2 py-4">
        <div className="flex justify-end mb-4">
          <button onClick={e => { if(this.textArea.current) { this.textArea.current.focus(); this.textArea.current.select(); document.execCommand('copy'); } } }><Icon type="copy" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Copy to clipboard</button>
        </div>
        <textarea ref={this.textArea} readOnly className="border p-2 w-full leading-tight bg-grey-lighter shadow-inner-top" style={{minHeight: `${items.length * 1.25 + 1.5}rem`}}>
          {items.map(i => `${i.productCode} ${i.itemQuantity}`).join('\n')}
        </textarea>
      </div>
  }
}