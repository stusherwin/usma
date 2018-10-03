import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, ProductCatalogueEntry, OrderItem } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { RouterLink } from './RouterLink'
import { Icon } from './Icon'
import { Money } from './Money'

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , loading: boolean
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => void
                                 }

export interface AddProductState { 
                                 }

export class AddProduct extends React.Component<AddProductProps, AddProductState> {
  constructor(props: AddProductProps) {
    super(props)
  }

  render() {
    return (
      <div className="bg-grey-transparent p-2 fixed z-20 pin-t pin-l w-full h-full"> 
        <div className="bg-white w-full h-full overflow-y-scroll">
          <div className="bg-product-light p-2">
            <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
              <h2 className="text-white leading-none mb-2 -mt-1">Add a product{this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
              <div className="flex justify-start">
                <button onClick={this.props.cancelAdd}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
              </div>
            </div>
          </div>
          <div className="bg-product-lightest p-2">
            <label htmlFor="search">Search for a particular product:</label>
            <div className="relative mt-2">
              <span className="absolute text-grey-darker" style={{bottom: '-2px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
              <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" className="w-full pl-6 input" /> {/*value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />*/}
            </div>
          </div>
          <div className="mb-4">
            {this.props.products.map((p, i) => 
              <div key={p.code} className="px-2 py-2 mb-4 mt-4 bg-white">
                <div className="flex justify-between items-baseline">
                  <span className="flex-no-shrink flex-no-grow font-bold">{p.code}</span>
                  <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={p.price} />
                </div>
                <p className="mt-2">{p.name}</p>
                <div className="flex justify-between items-end mt-2">
                  <span className="flex-no-shrink flex-no-grow text-grey">VAT: {p.vatRate} rate</span>
                  <button className="ml-2" onClick={_ => this.props.confirmAdd(p)}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    )
  }
}