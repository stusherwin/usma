import * as React from 'react';

import { CollectiveOrder } from 'util/Types'
import { Icon, IconType } from 'util/Icon'

interface Message { icon: IconType
                    text: string
                  }

export function getMessages(order: CollectiveOrder | undefined): Message[] {
  if(!order || order.orderIsPlaced || order.orderIsAbandoned)
    return []

  const allComplete = order.isComplete;
  const allHouseholdsUpToDate = order.allHouseholdsUpToDate;
  const orderMinimumReached = order.totalIncVat >= 25000

  return [
    {
      icon: !!order.householdOrders.length && allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info',
      text: !order.householdOrders.length?
          "Waiting for households to join"
        : !allHouseholdsUpToDate?
          "Waiting for all households to accept latest catalogue updates"
        : !orderMinimumReached?
          "Waiting for &pound;250.00 order minimum to be reached"
        : !allComplete?
          "Waiting for all orders to be completed"
        // : !allPaid?
        //   "Waiting for everyone to pay up"
        : "Waiting for admin to place order"
    }
  ]
}

interface OrderMessagesProps { order: CollectiveOrder | undefined
                             }

export const OrderMessages = ({order}: OrderMessagesProps) => {
  let messages = getMessages(order)

  if(!messages.length)
    return null

  return (
    <div>
      {messages.map(m => 
        <div className="flex bg-blue-lighter text-black p-2 py-4 shadow-inner-top">
          <Icon type={m.icon} className="flex-no-shrink w-4 h-4 mr-2 nudge-d-1 fill-current" />
          <span dangerouslySetInnerHTML={{ __html: m.text}}></span>
        </div>
      )}
    </div>
  )
}