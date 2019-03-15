import * as React from 'react';

import { RouterLink } from '../../RouterLink'
import { Icon } from '../../Icon'

export interface HomePageProps { 
                               }

export class HomePage extends React.Component<HomePageProps, {}> {
  render() {
    return (
      <div className="h-screen">
        <div className="collective-orders-header"><h1>Collective Orders</h1></div>
        <RouterLink className="bg-household-light p-2 h-1/3 text-household-darker block no-underline hover:text-household-dark hover:no-underline" path="/admin/households">
          <div className="flex w-4/5 mt-2 pr-2">
            <i className="text-3xl ml-2">#1</i>
            <p className="ml-4">Find some households you want to join in with...</p>
          </div>
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative mt-6 overflow-auto">
            <h2 className="leading-none mb-2 mt-4">Households <Icon type="right-arrow" className="w-4 h-4 fill-current nudge-d-1" /></h2>
          </div>
        </RouterLink>
        <RouterLink className="bg-product-light p-2 h-1/3 text-white block no-underline hover:text-product-lightest hover:no-underline" path="/admin/products">
          <div className="flex w-4/5 mt-2 pr-2">
            <i className="text-3xl ml-2">#2</i>
            <p className="ml-4">Have a list of products you want to buy...</p>
          </div>
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-6 overflow-auto">
            <h2 className="leading-none mb-2 mt-4">Products <Icon type="right-arrow" className="w-4 h-4 fill-current nudge-d-1" /></h2>
          </div>
        </RouterLink>
        <RouterLink className="bg-order-dark p-2 h-1/3 text-black block no-underline hover:text-grey-darkest hover:no-underline" path="/admin/orders">
          <div className="flex w-4/5 mt-2">
            <i className="text-3xl ml-2">#3</i>
            <p className="ml-4">Combine all your orders into one big order!</p>
          </div>
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mt-6 overflow-auto">
            <h2 className="leading-none mb-2 mt-4">Orders <Icon type="right-arrow" className="w-4 h-4 fill-current nudge-d-1" /></h2>
          </div>
        </RouterLink>
      </div>
    )
  }
}  