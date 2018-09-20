import * as React from 'react';
import * as classNames from 'classnames'

import { Product, VatRate } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { Button } from '../Button'
import { Icon } from '../Icon'
import { Money } from '../Money'
import { Form, Field, Validate } from '../Validation'
import { RouterLink } from '../RouterLink'
import { TopNav } from '../TopNav'
import { TextField, MoneyField, DropDownField } from '../Field'

const pageSize = 10
const loadMoretriggerMargin = 50
const maxPages = 3

interface IndexedProduct extends Product {
  index: number
}

const wrap = (offset: number) => (p: Product, i: number) => ({
  id: p.id,
  code: p.code,
  name: p.name,
  price: p.price,
  vatRate: p.vatRate,
  index: offset + i
})

export interface ProductsPageProps { products: Product[]
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   , loading: boolean
                                   , error: ApiError | null
                                   }

export interface ProductsPageState { loadMoreVisible: boolean
                                   , products: IndexedProduct[]
                                   , nextStartIndex: number
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, ProductsPageState> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { loadMoreVisible: false
                 , products: props.products.slice(0, pageSize).map(wrap(0))
                 , nextStartIndex: pageSize
                 }
  }

  componentDidMount() {
    const checkLoadMoreTriggered = () => {
      const loadMore = document.getElementById('load-more')
      if(!loadMore) return false
      
      const rect = loadMore.getBoundingClientRect()
      const loadMoreVisible = rect.top - window.innerHeight < loadMoretriggerMargin
      if(loadMoreVisible !== this.state.loadMoreVisible) {
        this.setState({loadMoreVisible})
        if(loadMoreVisible) {
          return true
        }
      }

      return false
    }
    
    checkLoadMoreTriggered()

    document.onscroll = () => {
      const loadMoreTriggered = checkLoadMoreTriggered()
      if(loadMoreTriggered) {
        const start = this.state.nextStartIndex
        const end = start + pageSize
        this.setState({ products: [...this.state.products, ...this.props.products.slice(start, end).map(wrap(start))]
                      , nextStartIndex: end
                      , loadMoreVisible: false
                      })
      }
    }
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
            <h2 className="text-white leading-none mb-2 -mt-1">Products{!!this.props.loading && <Icon type="refresh" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
          </div>
        </div>
        {!this.state.products.length
        ? <div className="p-2 mb-4 text-grey-darker"><Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No products available</div>
        : (
          <div>
            { this.state.products.map((p, i) => (
              <div key={p.index} className={classNames('px-2 py-2', /*,
                                                    {'border-grey-light border-t': i > 0 }*/
                                                    { 'mt-4': i > 0
                                                    , 'mb-4': i < this.state.products.length - 1})}>
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
        <div id="load-more" className="bg-grey-lightest py-8 text-center text-grey mt-4">
          <Icon type="refresh" className="w-4 h-4 mx-auto rotating ml-2 fill-current" />
        </div>
      </div>
    )
  }
}  