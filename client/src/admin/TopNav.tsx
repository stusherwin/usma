import * as React from 'react';
import * as classNames from 'classnames'

import { RouterLink } from '../common/RouterLink'
import { Router } from '../common/Router'

export interface TopNavProps {
  className?: string
}

export const TopNav = ({className}: TopNavProps) => {
  const links = [
    { 
      text: 'Orders',
      path: '/admin/orders',
      iconClassName: 'bg-img-order',
      className: 'bg-order-dark text-black hover:text-black',
      current: Router.isCurrent('/admin/orders'),
    },
    { 
      text: 'Products',
      path: '/admin/products',
      iconClassName: 'bg-img-product',
      className: 'bg-product-light text-white hover:text-white',
      current: Router.isCurrent('/admin/products'),
    },
    { 
      text: 'Households',
      path: '/admin/households',
      iconClassName: 'bg-img-household',
      className: 'bg-household-light',
      current: Router.isCurrent('/admin/households'),
    }
  ]

  return (
    <nav className={classNames('flex bg-grey-darker pt-1')}>
      {links.map((l, i) => 
        <RouterLink path={l.path} className={classNames('flex-grow h-8 relative pl-6 no-underline flex justify-center items-center', l.className, {
            'shadow-inner-bottom': !l.current,
            'rounded-t-lg': true,
            'ml-1': true,
            // 'border-l border-grey-darker': i == 0,
            // 'border-r border-grey-darker': i == links.length - 1,
            'mr-1': i == links.length - 1,
          })}>
          <div className={classNames('absolute pin-l pin-t bg-no-repeat w-6 h-6 ml-1 mt-1', l.iconClassName)}></div>
          <span className="">{l.text}</span>
        </RouterLink>
      )}
    </nav>
  )
}