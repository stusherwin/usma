import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, ProductCatalogueEntry, OrderItem } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../common/Util'
import { RouterLink } from '../common/RouterLink'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { LoadMore } from '../common/LoadMore'

const pageSize = 10

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , loading: boolean
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => void
                                 }

export interface AddProductState { products: ProductCatalogueEntry[]
                                 , filteredProducts: ProductCatalogueEntry[]
                                 , nextStartIndex: number
                                 , searchString: string
                                 , showLoadMore: boolean
                                 , loadMoreScrollElement: HTMLElement | null
                                 }

export class AddProduct extends React.Component<AddProductProps, AddProductState> {
  constructor(props: AddProductProps) {
    super(props)

    const filteredProducts = props.products

    this.state = { filteredProducts
                 , products: filteredProducts.slice(0, pageSize)
                 , nextStartIndex: pageSize
                 , searchString: ''
                 , showLoadMore: false
                 , loadMoreScrollElement: null
                 }
  }

  componentDidMount() {
    const elem = document.getElementById('add-container')
    this.setState({ showLoadMore: true
                  , loadMoreScrollElement: elem
                  })
  }

  loadMore = () => {
    const start = this.state.nextStartIndex
    const end = start + pageSize
    const moreProducts = this.state.filteredProducts.slice(start, end)

    this.setState({ products: [...this.state.products, ...moreProducts]
                  , nextStartIndex: end
                  , showLoadMore: moreProducts.length >= pageSize
                  })
  }

  searchChanged = (value: string) => {
    const searchString = value.toLowerCase()
    const searchWords = searchString.split(' ')
    const searchFilter = (p: ProductCatalogueEntry) => {
      const code = p.code.toLowerCase()
      const name = p.name.toLowerCase()
      return searchWords.every(w => code.includes(w) || name.includes(w))
    }
      
    const filteredProducts = !!searchString.length
      ? this.props.products.filter(searchFilter)
      : this.props.products

    this.resetFilteredProducts(searchString, filteredProducts)
  }

  resetFilteredProducts = (searchString: string, filteredProducts: ProductCatalogueEntry[]) => {
    this.setState({ filteredProducts
                  , searchString
                  , products: filteredProducts.slice(0, pageSize)
                  , nextStartIndex: pageSize
                  , showLoadMore: filteredProducts.length >= pageSize
                  })
  }

  render() {
    return (
      <div className="bg-grey-transparent p-2 fixed z-20 pin-t pin-l w-full h-full"> 
        <div id="add-container" className="bg-white w-full h-full overflow-y-scroll">
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
              <span className="absolute text-grey-darker" style={{bottom: '0px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
              <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />
            </div>
          </div>
          <div className="mb-4">
            {this.state.products.map((p, i) => 
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
          {this.state.showLoadMore && this.state.loadMoreScrollElement &&
            <LoadMore scrollElement={this.state.loadMoreScrollElement} loadMore={this.loadMore} />
          }
        </div>
      </div>
    )
  }
}