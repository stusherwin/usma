import * as React from 'react';

import { RouterLink } from '../RouterLink'

export interface HomePageProps { 
                               }

export class HomePage extends React.Component<HomePageProps, {}> {
  render() {
    return (
      <div className="h-screen">
        <div className="collective-orders-header"><h1>Collective Orders</h1></div>
        <div className="bg-household-light p-2 h-1/3">
          <div className="bg-img-household bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2 overflow-auto">
            <h2 className="leading-none mb-2 mt-4 text-household-darker">Households</h2>
          </div>
          <div className="flex w-3/4 mt-2">
            <i className="text-3xl mt-2 ml-3">#1</i>
            <p className="ml-6">Find some <RouterLink path="/households" className="text-household-dark hover:text-household-darker">households</RouterLink> who want to work together...</p>
          </div>
        </div>
        <div className="bg-product-light p-2 h-1/3 text-white">
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2 overflow-auto">
            <h2 className="leading-none mb-2 mt-4">Products</h2>
          </div>
          <div className="flex w-3/4 mt-2">
            <i className="text-3xl mt-2 ml-3">#2</i>
            <p className="ml-6">Next, get a list of <RouterLink path="/products" className="text-white hover:text-product-lightest">products</RouterLink> they want to buy...</p>
          </div>
        </div>
        <div className="bg-order-dark p-2 h-1/3">
          <div className="bg-img-order bg-no-repeat bg-16 pl-20 min-h-16 relative mt-2 overflow-auto">
            <h2 className="leading-none mb-2 mt-4">Orders</h2>
          </div>
          <div className="flex w-3/4 mt-2">
            <i className="text-3xl mt-2 ml-3">#3</i>
            <p className="ml-6">Let them combine all their orders together into <RouterLink path="/orders">one big order</RouterLink>!</p>
          </div>
        </div>
      </div>
    )
  }
}  