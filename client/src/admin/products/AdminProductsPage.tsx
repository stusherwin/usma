import * as React from 'react';

import { ProductCatalogueEntry } from 'util/Types'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'
import { AdminTopNav } from 'admin/AdminTopNav'

import { ProductList } from 'product/ProductList'
import { FilteredProducts } from 'product/FilteredProducts'
import { ProductFilters } from 'product/ProductFilters'

export interface AdminProductsPageProps { products: ProductCatalogueEntry[]
                                        , categories: string[]
                                        , brands: string[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface AdminProductsPageState { uploading: boolean
                                        , uploadedFile: File | undefined
                                        , filteredProducts: FilteredProducts
                                        }

export class AdminProductsPage extends React.Component<AdminProductsPageProps, AdminProductsPageState> {
  constructor(props: AdminProductsPageProps) {
    super(props)

    this.state = { filteredProducts: new FilteredProducts(props.products, props.categories, this.props.brands)
                 , uploading: false
                 , uploadedFile: undefined
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

  startUpload = () => {
    this.setState({ uploading: true
                  , uploadedFile: undefined
                  , filteredProducts: new FilteredProducts(this.props.products, this.props.categories, this.props.brands)
                  })
  }

  confirmUpload = () => {
    if(!this.state.uploadedFile) return

    var formData = new FormData()
    formData.append('files', this.state.uploadedFile, this.state.uploadedFile.name)

    this.props.request(ServerApi.command.uploadProductCatalogue(formData))
      .then(this.props.reload)
      .then(_ => {
        this.setState({ uploading: false
                      , uploadedFile: undefined
                      , filteredProducts: new FilteredProducts(this.props.products, this.props.categories, this.props.brands)
                      })
      })
  }

  cancelUpload = () => {
    this.setState({ uploading: false
                  , uploadedFile: undefined
                  })
  }

  fileChanged = (file: File | undefined) => {
    this.setState({uploadedFile: file})
  }

  render() {
    return (
      <div className="bg-product-light min-h-screen">
        <AdminTopNav />
        <div className="bg-product-light p-2">
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="text-white leading-none mb-2 -mt-1">Products</h2>
            <div className="flex justify-start">
              <button onClick={this.startUpload} disabled={this.state.uploading}><Icon type="upload" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Upload product list</button>
            </div>
          </div>
        </div>
        {!this.state.uploading && 
          <ProductFilters searchString={this.state.filteredProducts.searchString}
                          flags={this.state.filteredProducts.flags}
                          categories={this.state.filteredProducts.categories}
                          brands={this.state.filteredProducts.brands}
                          searchChanged={this.searchChanged}
                          flagChanged={this.flagChanged}
                          categoryChanged={this.categoryChanged}
                          brandChanged={this.brandChanged} />
        }
        {this.state.uploading && 
          <div className="bg-product-lightest px-2 py-4 shadow-inner-top">
            <h3 className="mb-4">Upload product list</h3>
            <div className="field mb-4">
              <div className="flex justify-between items-baseline">
                <label className="flex-no-grow flex-no-shrink mr-2"
                       htmlFor="upload">Choose file: </label>
                <input type="file"
                       name="file"
                       id="upload"
                       className="flex-grow flex-no-shrink"
                       onChange={e => this.fileChanged((e.target.files || [])[0])} />
              </div>
            </div>
            <div className="flex justify-end">  
              <button className="ml-2" onClick={this.confirmUpload} disabled={!this.state.uploadedFile}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Upload</button>
              <button className="ml-2" onClick={this.cancelUpload}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
            </div>
          </div>
        }
        <ProductList products={this.state.filteredProducts.products}
                     cataloguePopulated={!!this.props.products.length} />
      </div>
    )
  }
}