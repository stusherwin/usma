import * as React from 'react';

import { ProductCatalogueEntry } from '../Types'
import { ServerApi } from '../ServerApi'
import { Icon } from '../common/Icon'
import { AdminTopNav } from '../components/AdminTopNav'
import { ProductList } from '../components/ProductList'

export interface AdminProductsPageProps { products: ProductCatalogueEntry[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

const emptyFlags = {'b': false, 'g': false, 'o': false, 'f': false, 'v': false, 's': false}
export interface ProductFlags {[key: string]: boolean }

export class FilteredProducts {
  allProducts: ProductCatalogueEntry[]
  products: ProductCatalogueEntry[]
  searchString: string
  flags: ProductFlags

  constructor(allProducts: ProductCatalogueEntry[], products?: ProductCatalogueEntry[], searchString: string = '', flags: ProductFlags = emptyFlags) {
    this.allProducts = allProducts;
    this.products = products || allProducts;
    this.searchString = searchString;
    this.flags = flags;
  }

  search = (searchString: string) => {
    return this.filter(searchString.toLowerCase(), this.flags);
  }

  toggleFlag = (changedFlag: string) => {
    let flags:ProductFlags = {}
    for(let f in this.flags) {
      flags[f] = f == changedFlag? !this.flags[f] : this.flags[f]
    }

    return this.filter(this.searchString, flags);
  } 

  filter = (searchString: string, flags: ProductFlags) => {
    if(!searchString.length && !flags['b'] && !flags['g'] && !flags['o'] && !flags['f'] && !flags['v'] && !flags['s']) {
      return new FilteredProducts(this.allProducts);
    }
      
    const filteredProducts = this.allProducts.filter((p: ProductCatalogueEntry) => {
      const code = p.code.toLowerCase()
      const name = p.name.toLowerCase()
      const searchWords = searchString.split(' ')
      
      return searchWords.every(w => code.includes(w) || name.includes(w))
          && (!flags['b'] || p.biodynamic)
          && (!flags['g'] || p.glutenFree)
          && (!flags['o'] || p.organic)
          && (!flags['f'] || p.fairTrade)
          && (!flags['v'] || p.vegan)
          && (!flags['s'] || p.addedSugar)
    })

    return new FilteredProducts(this.allProducts, filteredProducts, searchString, flags)
  }
}

export interface AdminProductsPageState { uploading: boolean
                                        , uploadedFile: File | undefined
                                        , filteredProducts: FilteredProducts
                                        }

export class AdminProductsPage extends React.Component<AdminProductsPageProps, AdminProductsPageState> {
  constructor(props: AdminProductsPageProps) {
    super(props)

    this.state = { filteredProducts: new FilteredProducts(props.products)
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

  startUpload = () => {
    this.setState({ uploading: true
                  , uploadedFile: undefined
                  , filteredProducts: new FilteredProducts(this.props.products)
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
                      , filteredProducts: new FilteredProducts(this.props.products)
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
          <div className="bg-product-light p-2">
            <label htmlFor="search" className="text-white">Search for a particular product:</label>
            <div className="relative mt-2">
              <span className="absolute text-grey-darker" style={{bottom: '0px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
              <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={this.state.filteredProducts.searchString} onChange={e => this.searchChanged(e.target.value)} />
            </div>
            <div className="mt-2 flex justify-between">
              <span className="whitespace-no-wrap" style={{flexBasis: '33.3%'}}><input type="checkbox" id="b" value="b" checked={this.state.filteredProducts.flags['b']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="b" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">B</span></span>iodynamic</label></span>
              <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}><input type="checkbox" id="g" value="g" checked={this.state.filteredProducts.flags['g']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="g" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">G</span></span>luten Free</label></span>
              <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}><input type="checkbox" id="o" value="o" checked={this.state.filteredProducts.flags['o']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="o" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">O</span></span>rganic</label></span>
            </div>
            <div className="mt-1 flex justify-between">            
              <span className="whitespace-no-wrap" style={{flexBasis: '33.3%'}}><input type="checkbox" id="f" value="f" checked={this.state.filteredProducts.flags['f']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="f" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">F</span></span>air Trade</label></span>
              <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}><input type="checkbox" id="v" value="v" checked={this.state.filteredProducts.flags['v']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="v" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">V</span></span>egan</label></span>
              <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}><input type="checkbox" id="s" value="s" checked={this.state.filteredProducts.flags['s']} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="s" className="text-white">Added <span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">S</span></span>ugar</label></span>
            </div>
          </div>
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