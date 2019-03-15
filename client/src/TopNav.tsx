import * as React from 'react';
import { RouterLink } from './RouterLink'

export interface TopNavProps {
  className?: string
}

export const TopNav = ({className}: TopNavProps) =>
  <nav className="flex justify-center mb-2">
    <RouterLink className={`${className} ml-2 mr-2`} path="/admin/orders">Orders</RouterLink>
    &middot;
    <RouterLink className={`${className} ml-2 mr-2`} path="/admin/products">Products</RouterLink>
    &middot;
    <RouterLink className={`${className} ml-2 mr-2`} path="/admin/households">Households</RouterLink>
  </nav>