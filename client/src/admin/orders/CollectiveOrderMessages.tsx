// import * as React from 'react';

// import { CollectiveOrder, HouseholdOrder } from 'util/Types'
// import { Icon } from 'util/Icon'

// export interface CollectiveOrderMessagesProps {
//   collectiveOrder: CollectiveOrder | undefined
// }

// export const CollectiveOrderMessages = ({collectiveOrder}: CollectiveOrderMessagesProps) => {
//   if(!collectiveOrder || collectiveOrder.orderIsPlaced || collectiveOrder.orderIsAbandoned)
//     return null

//   const allComplete = collectiveOrder.householdOrders.reduce((complete: boolean, ho: HouseholdOrder) => complete && !ho.isOpen, true)
//   const orderMinimumReached = collectiveOrder.totalIncVat >= 25000
//   const allHouseholdsUpToDate = collectiveOrder.allHouseholdsUpToDate;

//   return (
//     <div className="flex bg-blue-lighter text-black p-2 py-4 shadow-inner-top">
//       <Icon type={!!collectiveOrder.householdOrders.length && allHouseholdsUpToDate && orderMinimumReached && allComplete? 'ok' : 'info'} className="flex-no-shrink w-4 h-4 mr-2 nudge-d-1 fill-current" />
//       { !collectiveOrder.householdOrders.length?
//         <span>Waiting for households to join</span>
//       : !allHouseholdsUpToDate?
//         <span>Waiting for all households to accept latest catalogue updates</span>
//       : !orderMinimumReached?
//         <span>Waiting for &pound;250.00 order minimum to be reached</span>
//       : !allComplete?
//         <span>Waiting for all orders to be completed</span>
//         // : !allPaid?
//         //   <span className="text-blue"><Icon type="info" className="w-4 h-4 fill-current mr-1 nudge-d-2" />Waiting for everyone to pay up</span>
//       : <span>Waiting for admin to place order</span>
//       }
//     </div>
//   )
// }