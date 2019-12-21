import * as React from 'react';

import { ProductCatalogueEntry } from '../util/Types'
import { Icon } from '../common/Icon'
import { ProductList } from '../product/ProductList'
import { FilteredProducts } from '../product/FilteredProducts'
import { ProductFilters } from '../product/ProductFilters'

const pageSize = 10

export interface AddProductProps { products: ProductCatalogueEntry[]
                                 , categories: string[]
                                 , brands: string[]
                                 , cancelAdd: () => void
                                 , confirmAdd: (p: ProductCatalogueEntry) => Promise<void>
                                 }

export interface AddProductState { filteredProducts: FilteredProducts
                                 , addedProductCodes: string[]
                                 }

export class AddProduct extends React.Component<AddProductProps, AddProductState> {
  constructor(props: AddProductProps) {
    super(props)

    this.state = { filteredProducts: new FilteredProducts(props.products, props.categories, props.brands)
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

  categoryChanged = (changedCategory: string | null) => {
    this.setState({ filteredProducts: this.state.filteredProducts.byCategory(changedCategory) });
  }

  brandChanged = (changedBrand: string | null) => {
    this.setState({ filteredProducts: this.state.filteredProducts.byBrand(changedBrand) });
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
        <div className="bg-product-light text-white p-2 relative shadow-inner-top">
          <div className="bg-img-product bg-no-repeat w-16 h-16 absolute"></div>
          <h2 className="leading-none ml-20">Add items</h2>
          <div className="ml-20 mt-3">
            <button onClick={this.props.cancelAdd}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Done</button>
          </div>
        </div>
        <ProductFilters searchString={this.state.filteredProducts.searchString}
                        flags={this.state.filteredProducts.flags}
                        categories={this.state.filteredProducts.categories}
                        brands={this.state.filteredProducts.brands}
                        category={this.state.filteredProducts.category}
                        brand={this.state.filteredProducts.brand}
                        searchChanged={this.searchChanged}
                        flagChanged={this.flagChanged}
                        categoryChanged={this.categoryChanged}
                        brandChanged={this.brandChanged} />
        <ProductList products={this.state.filteredProducts.products}
                     cataloguePopulated={!!this.props.products.length}
                     addProduct={this.confirmAdd} />
      </div>
    )
  }
}