import * as React from 'react';
import * as classNames from 'classnames'

import { CollectiveOrder } from '../../util/Types'
import { ServerApi } from '../../util/ServerApi'
import { Money } from '../../util/Money'
import { Icon } from '../../util/Icon'

import { CurrentCollectiveOrderItem } from './CurrentCollectiveOrderItem'

export interface CurrentCollectiveOrderItemsProps { collectiveOrder: CollectiveOrder
                                        }

export interface CurrentCollectiveOrderItemsState {
                                        }
                                     
export class CurrentCollectiveOrderItems extends React.Component<CurrentCollectiveOrderItemsProps, CurrentCollectiveOrderItemsState> {
  render() {
    const order = this.props.collectiveOrder
  
    return !order.items.length?
      <div className="px-2 py-4 text-grey-darker">
        <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items yet
      </div>
    : <div>
        <div className="flex justify-end mr-2 mt-4 mb-2">
          <button className="flex-no-grow flex-no-shrink" onClick={e => document.location.href = ServerApi.url("query/collective-order-download/")}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
        </div>
        <table className="border-collapse w-full">
          {this.props.collectiveOrder.items.map((item, index) => 
            <CurrentCollectiveOrderItem item={item} index={index} />
          )}
          <tr>
            <td></td>
            <td className={classNames('pt-4 align-baseline px-2')} colSpan={4}>
              <div className="flex justify-between">
                <span>VAT:</span>
                <span className={classNames('text-right')}>
                  <span><Money amount={order.totalIncVat - order.totalExcVat} /></span>
                </span>
              </div>
            </td>
          </tr>
          <tr>
            <td></td>
            <td className={classNames('pt-4 align-baseline px-2 pb-4')} colSpan={4}>
              <div className="flex justify-between font-bold">
                <span>Total:</span>
                <span className="text-right">
                  <Money className="flex-no-shrink flex-no-grow text-right" amount={order.totalIncVat} />
                </span>
              </div>
            </td>
          </tr>
        </table>
      </div>
  }
}