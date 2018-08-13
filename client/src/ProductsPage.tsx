import * as React from 'react';

import { Product } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Link } from './Link'
import { Money } from './Money'
import { Validator, Form } from './Validator'

export interface ProductsPageProps { request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , navigate: (location: string) => void
                                   }

export interface ProductsPageState { products: Product[]
                                   , initialised: boolean
                                   , creating: boolean
                                   , form: Form
}

export class ProductsPage extends React.Component<ProductsPageProps, ProductsPageState> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { products: []
                 , initialised: false
                 , creating: false
                 , form: Validator.form({ name: Validator.field('', (v: string) => v,
                                          [ { validate: (v: string) => !!v.length, error: 'Name is required' }
                                          ])
                                        , price: Validator.field('0.00', (v: string) => (parseFloat(v) || 0) * 100,
                                          [ { validate: (v: string) => !!v.length, error: 'Price is required' }
                                          , { validate: (v: string) => parseFloat(v) !== NaN, error: 'Price must be a decimal value' }
                                          , { validate: (v: string) => parseFloat(v) > 0, error: 'Price must be greater than zero' }
                                          ])
                                        })
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

  startCreate = () => this.setState({ creating: true
                                    })

  cancelCreate = () => this.setState({ creating: false
                                     , form: Validator.reset(this.state.form)
                                     })

  confirmCreate = () => {
    const validated = Validator.validate(this.state.form)
    this.setState({ form: validated })

    if(validated.valid) {
      this.props.request(ServerApi.command.createProduct(validated.fields.name.value, validated.fields.price.value))
        .then(() => this.props.request(ServerApi.query.products()))
        .then(products => this.setState({ products
                                        , creating: false
                                        , form: Validator.reset(this.state.form)
                                        }))
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: Validator.update(this.state.form, fieldName, event.target.value) })
  
  delete = (p: Product) => 
    this.props.request(ServerApi.command.archiveProduct(p.id))
      .then(() => this.props.request(ServerApi.query.products()))
      .then(products => this.setState({ products
                                      }))

  render() {
    if(!this.state.initialised) return <div>Initialising...</div>
    
    return (
      <div>
        <h1>Products</h1>
        <Link action={this.startCreate}>New product</Link>
        {this.state.creating &&
          <div>
            <input type="text" value={this.state.form.fields.name.stringValue} className={this.state.form.validating && !this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
            <input type="text" value={this.state.form.fields.price.stringValue} className={this.state.form.validating && !this.state.form.fields.price.valid? 'invalid': 'valid'} onChange={this.fieldChanged('price')} />
            <Link action={this.confirmCreate} disabled={this.state.form.validating && !this.state.form.valid}>Add</Link>
            <Link action={this.cancelCreate}>Cancel</Link>
          </div>
        }
        {!this.state.products.length ? <div>No products</div> : (
          <div>
            { this.state.products.map(p => (
              <div key={p.id}>
                <span>{p.name}</span>
                <Money amount={p.price} />
                <Link action={() => this.delete(p)}>Delete</Link>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}