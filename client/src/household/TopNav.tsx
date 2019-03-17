import * as React from 'react';
import { RouterLink } from '../common/RouterLink'

export interface TopNavProps {
}

export const TopNav = (props: {householdId: number}) =>
  <nav className="flex justify-between bg-household-light">
    <RouterLink className="p-2 pb-3 text-center flex-grow bg-order-dark rounded-t-lg text-black" path={`/households/${props.householdId}/orders`}>Orders</RouterLink>
    <RouterLink className="p-2 pb-3 text-center flex-grow"  path={`/households/${props.householdId}/products`}>Products</RouterLink>
    <RouterLink className="p-2 pb-3 text-center flex-grow" path={`/households/${props.householdId}/households`}>Households</RouterLink>
  </nav>