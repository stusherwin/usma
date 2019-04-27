import * as React from 'react';
import * as classNames from 'classnames'

import { HouseholdOrder, ProductCatalogueEntry, OrderItem } from '../Types'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { LoadMore } from '../common/LoadMore'

const pageSize = 10

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , loading: boolean
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => Promise<void>
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
    this.setState({ showLoadMore: true
                  , loadMoreScrollElement: document.body
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

  confirmAdd = (p: ProductCatalogueEntry) => {
    this.props.confirmAdd(p)
      .then(() => this.resetFilteredProducts(this.state.searchString, this.state.filteredProducts.filter(fp => fp.code != p.code)))
  }

  render() {
    return (
      <div id="add-container">
        <div className="bg-product-light text-white p-2 relative">
          <div className="bg-img-product bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20">Add items</h2>
          <label className="block mt-4 ml-20" htmlFor="search">Search for a particular product:</label>
          <div className="relative mt-4">
            <span className="absolute text-grey-darker" style={{bottom: '0px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
            <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />
          </div>
          <div className="absolute pin-r pin-t mt-2 mr-2">
            <button onClick={this.props.cancelAdd}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Close</button>
          </div>
        </div>
        <div className="py-4 px-2 shadow-inner-top bg-white">
          { !this.state.products.length
            ? <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products found</div>
            : this.state.products.map((p, i) => 
              <div key={p.code} className={classNames('bg-white', {'mt-8': i > 0})}>
                <div className="flex justify-between items-baseline">
                  <span className="flex-no-shrink flex-no-grow font-bold">{p.code}</span>
                  <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={p.priceExcVat} />
                </div>
                <p className="mt-2">{p.name}</p>
                <div className="flex justify-between items-end mt-2">
                  <span className="flex-no-shrink flex-no-grow text-grey">VAT: {p.vatRate} rate</span>
                  <button className="ml-2" onClick={_ => this.confirmAdd(p)}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button>
                </div>
              </div>
            )
          }
        </div>
        {this.state.showLoadMore && this.state.loadMoreScrollElement &&
          <LoadMore scrollElement={this.state.loadMoreScrollElement} loadMore={this.loadMore} />
        }
      </div>
    )
  }
}