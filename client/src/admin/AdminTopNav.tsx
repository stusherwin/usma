import * as React from 'react';
import * as classNames from 'classnames'

import { RouterLink } from 'util/RouterLink'
import { Router } from 'util/Router'

export const AdminTopNav = () => {
  const links = [
    {
      text: 'Orders',
      path: '/admin/orders',
      icon: 'order',
      className: 'bg-order-dark text-black hover:text-black',
      current: Router.isCurrent('/admin/orders') || Router.isCurrentExact('/admin'),
    },
    {
      text: 'Products',
      path: '/admin/products',
      icon: 'product',
      className: 'bg-product-light text-white hover:text-white',
      current: Router.isCurrent('/admin/products'),
    },
    {
      text: 'Households',
      path: '/admin/households',
      icon: 'household',
      className: 'bg-household-light text-black hover:text-black',
      current: Router.isCurrent('/admin/households'),
    }
  ]

  return (
    <nav className={classNames('flex bg-grey-darker pt-1')}>
      {links.map((l, i) =>
        <RouterLink path={l.path} className={classNames('flex-grow h-8 relative no-underline flex justify-center items-center', l.className, {
          'shadow-inner-bottom': !l.current,
          'rounded-tl-lg ml-1': i > 0,
          'rounded-tr-lg': i < links.length - 1,
        })}>
          <svg className="w-6 h-6 mr-2">
            <use xlinkHref={"#icon-" + l.icon} />
          </svg>
          <span className="">{l.text}</span>
        </RouterLink>
      )}
    </nav>
  )
}