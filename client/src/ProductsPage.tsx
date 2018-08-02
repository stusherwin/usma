import * as React from 'react';

import { Product } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Money } from './Money'

export interface ProductsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , navigate: (location: string) => void
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, { products: Product[], initialised: boolean }> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { products: []
                 , initialised: false
                 }
  }

  componentDidMount() {
    this.props.request(ServerApi.query.products())
      .then(products => {
        this.setState({ products
                      , initialised: true
                      })
      })
  }

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    return (
      <div>
        <h1>Products</h1>
        {!this.state.products.length ? <div>No products</div> : (
          <div>
            { this.state.products.map(p => (
              <div key={p.id}>
                <span>{p.name}</span>
                <Money amount={p.price} />
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}