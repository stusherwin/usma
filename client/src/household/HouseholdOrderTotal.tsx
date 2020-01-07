import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder } from '../util/Types'
import { Money } from '../util/Money'

export const HouseholdOrderTotal = ({householdOrder}: {householdOrder: HouseholdOrder | null}) => {
  return !householdOrder?
    <Money amount={0} />
  : householdOrder.oldTotalIncVat === null || householdOrder.oldTotalIncVat === undefined || householdOrder.oldTotalIncVat == householdOrder.totalIncVat?
    <Money className={classNames({"line-through text-grey-darker": householdOrder.isAbandoned})} amount={householdOrder.totalIncVat} />
  : <span>
      <span className="line-through"><Money amount={householdOrder.oldTotalIncVat} /></span> 
      <Money className="text-red font-bold" amount={householdOrder.totalIncVat} />
    </span>
}