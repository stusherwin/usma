import * as React from 'react';
import { RouterLink } from '../common/RouterLink'

export interface TopNavProps {
}

export const TopNav = () =>
  <nav className="flex justify-between bg-household-light">
    <RouterLink className="p-2 pb-3 text-center flex-grow bg-order-dark rounded-t-lg text-black" path="/admin/orders">Orders</RouterLink>
    <RouterLink className="p-2 pb-3 text-center flex-grow" path="/admin/products">Products</RouterLink>
    <RouterLink className="p-2 pb-3 text-center flex-grow" path="/admin/households">Households</RouterLink>
  </nav>