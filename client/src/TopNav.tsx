import * as React from 'react';
import { RouterLink } from './RouterLink'

export const TopNav = () =>
  <nav className="flex justify-center mb-2">
    <RouterLink className="ml-2 mr-2" path="/orders">Orders</RouterLink>
    &middot;
    <RouterLink className="ml-2 mr-2" path="/products">Products</RouterLink>
    &middot;
    <RouterLink className="ml-2 mr-2" path="/households">Households</RouterLink>
  </nav>