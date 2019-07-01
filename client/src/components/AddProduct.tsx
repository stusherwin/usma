import * as React from 'react';

import { ProductCatalogueEntry } from '../Types'
import { Icon } from '../common/Icon'
import { ProductList } from './ProductList'
import { FilteredProducts } from '../common/FilteredProducts'
import { ProductFilters } from '../components/ProductFilters'

const pageSize = 10

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => Promise<void>
                                 }

export interface AddProductState { filteredProducts: FilteredProducts
                                 , addedProductCodes: string[]
                                 }

export class AddProduct extends React.Component<AddProductProps, AddProductState> {
  constructor(props: AddProductProps) {
    super(props)

    this.state = { filteredProducts: new FilteredProducts(props.products)
                 , addedProductCodes: []
                 }
  }

  searchChanged = (value: string) => {
    this.setState({ filteredProducts: this.state.filteredProducts.search(value)
                  })
  }

  flagChanged = (changedFlag: string) => {
    this.setState({ filteredProducts: this.state.filteredProducts.toggleFlag(changedFlag)
                  })
  }

  confirmAdd = (p: ProductCatalogueEntry) => {
    this.props.confirmAdd(p)
      .then(() => {
        let addedProductCodes = [...this.state.addedProductCodes, p.code]
        this.setState({ addedProductCodes
                      , filteredProducts: this.state.filteredProducts.filter(fp => addedProductCodes.indexOf(fp.code) == -1 )
                      })
      })
  }

  render() {
    return (
      <div id="add-container">
        <div className="bg-product-light text-white p-2 relative">
          <div className="bg-img-product bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20 pb-8">Add items</h2>
          <div className="absolute pin-r pin-t mt-2 mr-2">
            <button onClick={this.props.cancelAdd}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Close</button>
          </div>
        </div>
        <ProductFilters searchString={this.state.filteredProducts.searchString}
                        flags={this.state.filteredProducts.flags}
                        searchChanged={this.searchChanged}
                        flagChanged={this.flagChanged} />
        <ProductList products={this.state.filteredProducts.products}
                     cataloguePopulated={!!this.props.products.length}
                     addProduct={this.confirmAdd} />
      </div>
    )
  }
}