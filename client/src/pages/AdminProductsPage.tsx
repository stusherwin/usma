import * as React from 'react';
import * as classNames from 'classnames'

import { ProductCatalogueEntry, VatRate } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { AdminTopNav } from '../components/AdminTopNav'
import { LoadMore } from '../common/LoadMore'

const pageSize = 10

export interface AdminProductsPageProps { products: ProductCatalogueEntry[]
                                        , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                        , reload: () => Promise<void>
                                        }

export interface AdminProductsPageState { products: ProductCatalogueEntry[]
                                        , filteredProducts: ProductCatalogueEntry[]
                                        , nextStartIndex: number
                                        , searchString: string
                                        , showLoadMore: boolean
                                        , uploading: boolean
                                        , uploadedFile: File | undefined
                                        , b: boolean
                                        , g: boolean
                                        , o: boolean
                                        , f: boolean
                                        , v: boolean
                                        , s: boolean
                                        }

export class AdminProductsPage extends React.Component<AdminProductsPageProps, AdminProductsPageState> {
  constructor(props: AdminProductsPageProps) {
    super(props)

    const filteredProducts = props.products

    this.state = { filteredProducts
                 , products: filteredProducts.slice(0, pageSize)
                 , nextStartIndex: pageSize
                 , searchString: ''
                 , showLoadMore: false
                 , uploading: false
                 , uploadedFile: undefined
                 , b: false
                 , g: false
                 , o: false
                 , f: false
                 , v: false
                 , s: false
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

    this.resetFilteredProducts(searchString, this.state.b, this.state.g, this.state.o, this.state.f, this.state.v, this.state.s)
  }

  resetFilteredProducts = (searchString: string, b: boolean, g: boolean, o: boolean, f: boolean, v: boolean, s: boolean) => {
    const searchWords = searchString.split(' ')

    const searchFilter = (p: ProductCatalogueEntry) => {
      const code = p.code.toLowerCase()
      const name = p.name.toLowerCase()
      return searchWords.every(w => code.includes(w) || name.includes(w))
          && (!b || p.biodynamic)
          && (!g || p.glutenFree)
          && (!o || p.organic)
          && (!f || p.fairTrade)
          && (!v || p.vegan)
          && (!s || p.addedSugar)
    }
      
    const filteredProducts = !!searchString.length || b || g || o || f || v || s
      ? this.props.products.filter(searchFilter)
      : this.props.products

    this.setState({ filteredProducts
                  , searchString
                  , b, g, o, f, v, s
                  , products: filteredProducts.slice(0, pageSize)
                  , nextStartIndex: pageSize
                  , showLoadMore: filteredProducts.length >= pageSize
                  })
  }

  startUpload = () => {
    this.setState({ uploading: true
                  , uploadedFile: undefined
                  })
    this.resetFilteredProducts('', false, false, false, false, false, false)
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
        this.resetFilteredProducts('', false, false, false, false, false, false)
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

  flagChanged = (flag: string) => {
    let state:any = {};
    state[flag] = !((this.state as any)[flag] as boolean)
    let flagValue = (stateFlag: string) => {
      let stateFlagValue = (this.state as any)[stateFlag] as boolean
      let newFlag = flag == stateFlag ? !stateFlagValue : stateFlagValue
      return newFlag
    }
    this.resetFilteredProducts(this.state.searchString, flagValue('b'), flagValue('g'), flagValue('o'), flagValue('f'), flagValue('v'), flagValue('s'))
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
              <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={this.state.searchString} onChange={e => this.searchChanged(e.target.value)} />
            </div>
            <div className="mt-4 flex flex-wrap">
              <span className="w-1/3"><input type="checkbox" id="b" value="b" checked={this.state.b} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="b" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">B</span></span>iodynamic</label></span>
              <span className="w-1/3"><input type="checkbox" id="g" value="g" checked={this.state.g} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="g" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">G</span></span>luten Free</label></span>
              <span className="w-1/3"><input type="checkbox" id="o" value="o" checked={this.state.o} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="o" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">O</span></span>rganic</label></span>
              <span className="w-1/3"><input type="checkbox" id="f" value="f" checked={this.state.f} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="f" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">F</span></span>air Trade</label></span>
              <span className="w-1/3"><input type="checkbox" id="v" value="v" checked={this.state.v} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="v" className="text-white"><span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">V</span></span>egan</label></span>
              <span className="w-1/3"><input type="checkbox" id="s" value="s" checked={this.state.s} className="mr-1 nudge-d-1" onChange={e => this.flagChanged(e.target.value)} /><label htmlFor="s" className="text-white">Added <span className="inline-block text-center nudge-d-2 w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}><span className="nudge-u-2">S</span></span>ugar</label></span>
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
        <div className="py-4 px-2 shadow-inner-top bg-white">
          {!this.state.products.length
          ? <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />
              {this.props.products.length ? 'No matching products found' : 'Product catalogue is empty'}
            </div>
          : <table className="border-collapse w-full">
              {this.state.products.map((p, i) => 
                [
                <tr key={p.code + '-1'}>
                  <td className={classNames('w-20 h-20 align-top', {'pt-8': i > 0})} rowSpan={3}><img className="w-20 h-20 -ml-1" src={ServerApi.url(`query/product-image/${p.code}`)} /></td>
                  <td className={classNames('pb-2 font-bold align-baseline', {'pt-8': i > 0})} colSpan={3}>{p.code}</td>
                  <td className={classNames('pl-2 pb-2 text-right align-baseline', {'pt-8': i > 0})}><Money amount={p.priceExcVat} /></td>
                </tr>
                ,
                <tr key={p.code + '-2'}>
                  <td className={classNames('pb-2 align-top')} colSpan={3}>{p.name}</td>
                  <td className={classNames('pl-2 align-top text-right whitespace-no-wrap')} colSpan={1}>
                    {/* <button className="ml-2" onClick={_ => this.confirmAdd(p)}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button> */}
                  </td>
                </tr>
                ,
                <tr key={p.code + '-3'}>
                  <td className={classNames('text-grey')} colSpan={2}>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.biodynamic, "text-grey": !p.biodynamic })}><span className="inline-block nudge-u-2">B</span></span>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.glutenFree, "text-grey": !p.glutenFree })}><span className="inline-block nudge-u-2">G</span></span>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.organic,    "text-grey": !p.organic    })}><span className="inline-block nudge-u-2">O</span></span>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.fairTrade,  "text-grey": !p.fairTrade  })}><span className="inline-block nudge-u-2">F</span></span>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.vegan,      "text-grey": !p.vegan      })}><span className="inline-block nudge-u-2">V</span></span>
                    <span className={classNames("inline-block mr-1 w-4 h-4 text-center nudge-d-2", { "text-white bg-grey": p.addedSugar, "text-grey": !p.addedSugar })}><span className="inline-block nudge-u-2">S</span></span>
                  </td>
                  <td className={classNames('text-grey')} colSpan={1}>VAT: {p.vatRate} rate</td>
                  <td className={classNames('pl-2')}>&nbsp;</td>
                </tr>
                ])
              }
            </table>
          }
        </div>
        {this.state.showLoadMore && 
          <LoadMore scrollElement={document} loadMore={this.loadMore} />
        }
      </div>
    )
  }
}