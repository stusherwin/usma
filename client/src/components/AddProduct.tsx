import * as React from 'react';
import * as classNames from 'classnames'

import { ProductCatalogueEntry } from '../Types'
import { Icon } from '../common/Icon'
import { ProductList } from '../components/ProductList'

const pageSize = 10

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => Promise<void>
                                 }

export interface AddProductState { products: ProductCatalogueEntry[]
                                 , filteredProducts: ProductCatalogueEntry[]
                                 , searchString: string
                                 }

export class AddProduct extends React.Component<AddProductProps, AddProductState> {
  constructor(props: AddProductProps) {
    super(props)

    const filteredProducts = props.products

    this.state = { filteredProducts
                 , products: filteredProducts.slice(0, pageSize)
                 , searchString: ''
                 }
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
          <label className="block mt-4 ml-20 pt-2" htmlFor="search">Search for a particular product:</label>
          <div className="relative mt-2">
            <span className="absolute text-grey-darker" style={{bottom: '0px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
            <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />
          </div>
          <div className="absolute pin-r pin-t mt-2 mr-2">
            <button onClick={this.props.cancelAdd}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Close</button>
          </div>
        </div>
        <ProductList products={this.state.filteredProducts}
                     cataloguePopulated={!!this.props.products.length}
                     addProduct={this.confirmAdd} />
      </div>
    )
  }
}