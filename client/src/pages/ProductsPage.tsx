import * as React from 'react';
import * as classNames from 'classnames'

import { ProductCatalogueEntry, VatRate } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { Icon } from '../Icon'
import { Money } from '../Money'
import { Form, Field, Validate } from '../Validation'
import { RouterLink } from '../RouterLink'
import { TopNav } from '../TopNav'
import { TextField, MoneyField, DropDownField } from '../Field'
import { LoadMore } from '../LoadMore'

const pageSize = 10
const maxPages = 3

export interface ProductsPageProps { products: ProductCatalogueEntry[]
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   , loading: boolean
                                   , error: ApiError | null
                                   }

export interface ProductsPageState { products: ProductCatalogueEntry[]
                                   , filteredProducts: ProductCatalogueEntry[]
                                   , nextStartIndex: number
                                   , searchString: string
                                   , showLoadMore: boolean
                                   , uploading: boolean
                                   , uploadedFile: File | undefined
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, ProductsPageState> {
  constructor(props: ProductsPageProps) {
    super(props)

    const filteredProducts = props.products

    this.state = { filteredProducts
                 , products: filteredProducts.slice(0, pageSize)
                 , nextStartIndex: pageSize
                 , searchString: ''
                 , showLoadMore: false
                 , uploading: false
                 , uploadedFile: undefined
                 }
  }

  componentDidMount() {
    this.setState({showLoadMore: true})
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

  startUpload = () => {
    this.setState({ uploading: true
                  , uploadedFile: undefined
                  })
    this.resetFilteredProducts('', this.props.products)
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
                      })
        this.resetFilteredProducts('', this.props.products)
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
      <div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-product-light p-2">
          <TopNav className="text-white hover:text-white" />
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="text-white leading-none mb-2 -mt-1">Products{!!this.props.loading && <Icon type="loading" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <div className="flex justify-start">
              <button onClick={this.startUpload} disabled={this.state.uploading}><Icon type="upload" className="w-4 h-4 mr-2 fill-current nudge-d-2" />Upload product list</button>
            </div>
          </div>
        </div>
        {!this.state.uploading && 
          <div className="bg-product-lightest p-2">
            <label htmlFor="search">Search for a particular product:</label>
            <div className="relative mt-2">
              <span className="absolute text-grey-darker" style={{bottom: '-2px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
              <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" className="w-full pl-6 input" value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />
            </div>
          </div>
        }
        {this.state.uploading && 
          <div className="bg-product-lightest p-2">
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
        {!this.state.products.length
        ? <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />
            {this.state.searchString.length ? 'No matching products available' : 'No products loaded yet'}
          </div>
        : (
          <div>
            { this.state.products.map((p, i) => (
              <div key={p.code} className={classNames('px-2 py-2 mb-4',
                                                    { 'mt-4': i > 0
                                                    })}>
                <div className="flex justify-between items-baseline">
                  <span className="flex-no-shrink flex-no-grow font-bold">{p.code}</span>
                  <Money className="flex-no-shrink flex-no-grow text-right font-bold" amount={p.price} />
                </div>
                <p className="mt-2">{p.name}</p>
                <div className="flex justify-between items-end mt-2">
                  <span className="flex-no-shrink flex-no-grow text-grey">VAT: {p.vatRate} rate</span>
                </div>
              </div>
            )) }
          </div>
        )}
        {this.state.showLoadMore && 
          <LoadMore loadMore={this.loadMore} />
        }
      </div>
    )
  }
}