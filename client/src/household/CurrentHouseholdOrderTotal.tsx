import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder } from '../util/Types'
import { Money } from '../util/Money'

export const CurrentHouseholdOrderTotal = ({currentHouseholdOrder}: {currentHouseholdOrder: HouseholdOrder | null}) => {
  return !currentHouseholdOrder?
    <Money amount={0} />
  : currentHouseholdOrder.oldTotalIncVat === null || currentHouseholdOrder.oldTotalIncVat === undefined || currentHouseholdOrder.oldTotalIncVat == currentHouseholdOrder.totalIncVat?
    <Money className={classNames({"line-through text-grey-darker": currentHouseholdOrder.isAbandoned})} amount={currentHouseholdOrder.totalIncVat} />
  : <span>
      <span className="line-through"><Money amount={currentHouseholdOrder.oldTotalIncVat} /></span> 
      <Money className="text-red font-bold" amount={currentHouseholdOrder.totalIncVat} />
    </span>
}