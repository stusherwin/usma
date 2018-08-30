import * as React from 'react';
import { RouterLink } from './RouterLink'

export const TopNav = () =>
  <nav>
    <RouterLink path="/orders">Orders</RouterLink>
    <RouterLink path="/products">Products</RouterLink>
    <RouterLink path="/households">Households</RouterLink>
  </nav>