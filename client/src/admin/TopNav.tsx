import * as React from 'react';
import * as classNames from 'classnames'

import { RouterLink } from '../common/RouterLink'
import { Router } from '../common/Router'

export interface TopNavProps {
  className?: string
}

export const TopNav = ({className}: TopNavProps) => {
  var links = [
    { path: '/admin/orders',
      link: <RouterLink className={classNames(className, 'ml-2 mr-2')} path="/admin/orders">Orders</RouterLink>,
      icon: 'bg-img-order',
      bg:   'bg-order-dark' },
    { path: '/admin/products',
      link: <RouterLink className={classNames(className, 'ml-2 mr-2')} path="/admin/products">Products</RouterLink>,
      icon: 'bg-img-product',
      bg:   'bg-product-light' },
    { path: '/admin/households',
      link: <RouterLink className={classNames(className, 'ml-2 mr-2')} path="/admin/households">Households</RouterLink>,
      icon: 'bg-img-household',
      bg:   'bg-household-light' }
    ,
    
  ]
  const currentIndex = links.findIndex(link => Router.isCurrent(link.path));
  for(var i = 0; i < currentIndex; i++) {
    const l = links.shift();
    if(l) links.push(l);
  }
  return (
  <nav className={classNames('flex justify-between py-2 relative', className)}>
    <div className="pl-8">
      <div className={classNames('w-8 h-8 rounded-r-full absolute pin-l', links[1].bg)}><div className={classNames('bg-no-repeat w-6 h-6 ml-1 mt-1', links[1].icon)}></div></div>
      <div className="pt-1">{links[1].link}&gt;</div>
    </div>
    <div className="pr-8">
      <div className={classNames('w-8 h-8 rounded-l-full absolute pin-r', links[2].bg)}><div className={classNames('bg-no-repeat w-6 h-6 ml-1 mt-1', links[2].icon)}></div></div>
      <div className="pt-1">&lt;{links[2].link}</div>
    </div>
  </nav>
  )
}