import * as React from 'react';
import * as classNames from 'classnames'

import { ProductCatalogueEntry } from '../util/Types'
import { ServerApi} from '../util/ServerApi'
import { Icon } from '../util/Icon'
import { Money } from '../util/Money'
import { LoadMore } from '../util/LoadMore'

import { ProductFlags } from './ProductFlags'

const pageSize = 10

export interface ProductListProps { products: ProductCatalogueEntry[]
                                  , cataloguePopulated: boolean
                                  , addProduct?: (p: ProductCatalogueEntry) => void
                                  }

export interface ProductListState { products: ProductCatalogueEntry[]
                                  , nextStartIndex: number
                                  , showLoadMore: boolean
                                  }

export class ProductList extends React.Component<ProductListProps, ProductListState> {
  constructor(props: ProductListProps) {
    super(props)

    this.state = this.getPageState(0, [])
  }

  componentDidUpdate(prevProps: ProductListProps) {
    if(prevProps == this.props) return

    this.setState(this.getPageState(0, []))
  }

  loadMore = () => {
    this.setState(this.getPageState(this.state.nextStartIndex, this.state.products))
  }

  getPageState = (start: number, prevProducts: ProductCatalogueEntry[]) => {
    const end = start + pageSize

    return { products: [...prevProducts, ...this.props.products.slice(start, end)]
           , nextStartIndex: end
           , showLoadMore: this.props.products.slice(end + 1, end  + 1 + pageSize).length > 0
           }
  }

  render() {
    return (
      <div>
        <div className="py-4 px-2 shadow-inner-top bg-white">
          {!this.props.products.length
          ? <div className="text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />
              {this.props.cataloguePopulated ? 'No matching products found' : 'Product catalogue is empty'}
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
                  <td className={classNames('pb-2 align-top')} colSpan={this.props.addProduct? 2: 4}>{p.name}</td>
                  {this.props.addProduct && 
                    <td className={classNames('pl-2 align-top text-right whitespace-no-wrap')} colSpan={2}>
                      <button className="ml-2" onClick={_ => this.props.addProduct && this.props.addProduct(p)}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Add</button>
                    </td>
                  }
                </tr>
                ,
                <tr key={p.code + '-3'}>
                  <td className={classNames('text-grey')} colSpan={4}>
                    <ProductFlags p={p} />
                    <span className={classNames('text-grey pl-4 whitespace-no-wrap')}>VAT: {p.vatRate} rate</span>
                  </td>
                </tr>
                ])
              }
            </table>
          }
        </div>
        {this.state.showLoadMore && 
          <LoadMore scrollElement={document.body} loadMore={this.loadMore} />
        }
      </div>
    )
  }
}