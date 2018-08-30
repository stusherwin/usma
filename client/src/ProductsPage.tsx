import * as React from 'react';

import { Product } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { Util } from './Util'
import { Button } from './Button'
import { Money } from './Money'
import { Form, Field, Validate } from './Validation'
import { RouterLink } from './RouterLink'

export interface ProductsPageProps { products: Product[]
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   , loading: boolean
                                   , error: ApiError | null
                                   }

export interface ProductsPageState { creating: boolean
                                   , editingId: number | null
                                   , form: Form
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, ProductsPageState> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { creating: false
                 , editingId: null
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v,
                                         [ Validate.required('Name is required')
                                         ])
                                     , price: Field.create((v: string) => (parseFloat(v) || 0) * 100, (v: number) => (v / 100.0).toFixed(2),
                                         [ Validate.required('Price is required')
                                         , Validate.decimal('Price must be a number')
                                         , Validate.twoDP('Price can\'t have more than 2 decimal places')
                                         , Validate.greaterThanZero('Price must be more than zero')
                                         ])
                                     })
                 }
  }

  startCreate = () => this.setState({ creating: true
                                    })

  cancelCreate = () => this.setState({ creating: false
                                     , form: this.state.form.reset({name: '', price: 0})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.createProduct(validated.fields.name.value, validated.fields.price.value))
        .then(this.props.reload)
        .then(_ => this.setState({ creating: false
                                 , form: this.state.form.reset({name: '', price: 0})
                                 })
        )
    }
  }

  startEdit = (product: Product) => this.setState({ editingId: product.id
                                                  , form: this.state.form.reset({name: product.name, price: product.price})
                                                  })

  cancelEdit = () => {
    this.setState({ editingId: null
                  , form: this.state.form.reset({name: '', price: 0})
                  })
  }

  confirmEdit = () => {
    if(!this.state.editingId) return

    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.updateProduct(this.state.editingId, validated.fields.name.value, validated.fields.price.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editingId: null
                                 , form: this.state.form.reset({name: '', price: 0})
                                 }))
    }
  }

  fieldChanged = (fieldName: string) => (event: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ form: this.state.form.update(fieldName, event.target.value) })
  
  delete = (p: Product) => 
    this.props.request(ServerApi.command.archiveProduct(p.id))
      .then(this.props.reload)

  render() {
    return (
      <div>
        <div hidden={!this.props.loading}>Loading...</div>
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-img-product bg-no-repeat bg-16 pl-16 min-h-16">
          <div>
            <RouterLink path="/orders">Orders</RouterLink>
            <RouterLink path="/products">Products</RouterLink>
            <RouterLink path="/households">Households</RouterLink>
          </div>
          <h1>Products</h1>
          {!this.state.creating && 
            <Button action={this.startCreate}>New product</Button>
          }
        </div>
        {this.state.creating &&
          <div>
            <span>
              <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
              {this.state.form.fields.name.error}
            </span>
            <span>
              <input type="text" value={this.state.form.fields.price.stringValue} className={!this.state.form.fields.price.valid? 'invalid': 'valid'} onChange={this.fieldChanged('price')} />
              {this.state.form.fields.price.error}
            </span>
            <Button action={this.confirmCreate} disabled={!this.state.form.valid()}>Save</Button>
            <Button action={this.cancelCreate}>Cancel</Button>
          </div>
        }
        {!this.props.products.length
        ? <div>No products</div>
        : (
          <div>
            { this.props.products.map(p => 
            this.state.editingId == p.id
            ? (
              <div>
                <span>
                  <input type="text" value={this.state.form.fields.name.stringValue} className={!this.state.form.fields.name.valid? 'invalid': 'valid'} onChange={this.fieldChanged('name')} />
                  {this.state.form.fields.name.error}
                </span>
                <span>
                  <input type="text" value={this.state.form.fields.price.stringValue} className={!this.state.form.fields.price.valid? 'invalid': 'valid'} onChange={this.fieldChanged('price')} />
                  {this.state.form.fields.price.error}
                </span>
                <Button action={this.confirmEdit} disabled={!this.state.form.valid()}>Save</Button>
                <Button action={this.cancelEdit}>Cancel</Button>
              </div>
            )
            : (
              <div key={p.id}>
                <span>{p.name}</span>
                <Money amount={p.price} />
                <Button action={() => this.startEdit(p)}>Edit</Button>
                <Button action={() => this.delete(p)}>Delete</Button>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}  