import * as React from 'react';
import * as classNames from 'classnames'

import { Product } from '../Types'
import { ServerApi, ApiError } from '../ServerApi'
import { Util } from '../Util'
import { Button } from '../Button'
import { Icon } from '../Icon'
import { Money } from '../Money'
import { Form, Field, Validate } from '../Validation'
import { RouterLink } from '../RouterLink'
import { TopNav } from '../TopNav'

export interface ProductsPageProps { products: Product[]
                                   , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                   , reload: () => Promise<void>
                                   , loading: boolean
                                   , error: ApiError | null
                                   }

export interface ProductsPageState { editing: 'new' | number | null
                                   , form: Form
                                   }

export class ProductsPage extends React.Component<ProductsPageProps, ProductsPageState> {
  constructor(props: ProductsPageProps) {
    super(props)

    this.state = { editing: null
                 , form: Form.create({ name: Field.create((v: string) => v, (v: string) => v,
                                         [ Validate.required('Name is required')
                                         ])
                                     , price: Field.create((v: string) => (parseFloat(v) || 0) * 100, (v: number | null) => !v? '' : (v / 100.0).toFixed(2),
                                         [ Validate.required('Price is required')
                                         , Validate.decimal('Price must be a number')
                                         , Validate.twoDP('Price can\'t have more than 2 decimal places')
                                         , Validate.greaterThanZero('Price must be more than zero')
                                         ])
                                     })
                 }
  }

  startCreate = () => this.setState({ editing: 'new'
                                    , form: this.state.form.reset({name: '', price: ''})
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({name: '', price: ''})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.createProduct(validated.fields.name.value, validated.fields.price.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: '', price: ''})
                                 })
        )
    }
  }

  startEdit = (product: Product) => this.setState({ editing: product.id
                                                  , form: this.state.form.reset({name: product.name, price: product.price})
                                                  })

  cancelEdit = () => {
    this.setState({ editing: null
                  , form: this.state.form.reset({name: '', price: ''})
                  })
  }

  confirmEdit = () => {
    if(typeof this.state.editing !== 'number') return

    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.updateProduct(this.state.editing, validated.fields.name.value, validated.fields.price.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: '', price: ''})
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
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div className="bg-product-light p-2">
          <TopNav className="text-white hover:text-white" />
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4 overflow-auto">
            <h1 className="text-white leading-none mb-2 -mt-1 text-household-darker">Products{!!this.props.loading && <Icon type="refresh" className="w-4 h-4 rotating ml-2 fill-current" />}</h1>
            <div className="flex justify-start">
              <Button action={this.startCreate} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New product</Button>
            </div>
          </div>
        </div>
        {this.state.editing == 'new' &&
          <div className="bg-product-lightest p-2">
            <h3 className="mb-4">Create new product</h3>
            <div className={classNames('field mb-4', {'invalid': !this.state.form.fields.name.valid})}>
              <div className="flex justify-between items-baseline">
                <label className="flex-no-grow flex-no-shrink mr-2"
                       htmlFor="create-name">Name</label>
                <input type="text" 
                       id="create-name" 
                       autoFocus
                       className="flex-grow flex-no-shrink"
                       value={this.state.form.fields.name.stringValue} 
                       onChange={this.fieldChanged('name')} />
              </div>
              <div className="text-red mt-2" hidden={!this.state.form.fields.name.error}>
                {this.state.form.fields.name.error}
              </div>
            </div>
            <div className={classNames('field mb-4', {'invalid': !this.state.form.fields.price.valid})}>
              <div className="flex justify-between items-baseline">
                <label className="flex-no-grow flex-no-shrink mr-2"
                       htmlFor="create-price">Price</label>
                <span className="money-input flex-grow flex-no-shrink flex items-baseline">
                  <span className="flex-no-grow flex-no-shrink mr-1">&pound;</span>
                  <input type="text"
                         id="create-price"
                         className="flex-grow flex-no-shrink"
                         value={this.state.form.fields.price.stringValue}
                         onChange={this.fieldChanged('price')} />
                </span>
              </div>
              <div className="text-red mt-2" hidden={!this.state.form.fields.price.error}>
                {this.state.form.fields.price.error}
              </div>
            </div>
              <div className="flex justify-end items-baseline">
                <Button className="ml-2" action={this.confirmCreate} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</Button>
                <Button className="ml-2" action={this.cancelCreate}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</Button>
              </div>
          </div>
        }
        {!this.props.products.length
        ? <div>No products created yet</div>
        : (
          <div>
            { this.props.products.map((p, i) => 
            this.state.editing == p.id
            ? (
              <div key={p.id} className={classNames('bg-product-lightest p-2', {'mt-2': i > 0})}>
                <h3 className="mb-4">Edit product</h3>
                <div className={classNames('field mb-4', {'invalid': !this.state.form.fields.name.valid})}>
                  <div className="flex justify-between items-baseline">
                    <label className="flex-no-grow flex-no-shrink mr-2"
                           htmlFor="edit-name">Name</label>
                    <input type="text" 
                           id="edit-name" 
                           className="flex-grow flex-no-shrink"
                           autoFocus 
                           value={this.state.form.fields.name.stringValue} 
                           onChange={this.fieldChanged('name')} />
                  </div>
                  <div className="text-red mt-2" hidden={!this.state.form.fields.name.error}>
                    {this.state.form.fields.name.error}
                  </div>
                </div>
                <div className={classNames('field mb-4', {'invalid': !this.state.form.fields.price.valid})}>
                  <div className="flex justify-between items-baseline">
                    <label className="flex-no-grow flex-no-shrink mr-2"
                           htmlFor="edit-price">Price</label>
                    <span className="money-input flex-grow flex-no-shrink flex items-baseline">
                      <span className="flex-no-grow flex-no-shrink mr-1">&pound;</span>
                      <input type="text" 
                             id="edit-price" 
                             className="flex-grow flex-no-shrink"
                             value={this.state.form.fields.price.stringValue} 
                             onChange={this.fieldChanged('price')} />
                    </span>
                  </div>
                  <div className="text-red mt-2" hidden={!this.state.form.fields.price.error}>
                    {this.state.form.fields.price.error}
                  </div>
                </div>
                <div className="flex justify-end">
                  <Button className="ml-2" action={this.confirmEdit} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</Button>
                  <Button className="ml-2" action={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</Button>
                </div>
              </div>
            )
            : (
              <div key={p.id} className="flex justify-between items-baseline px-2 mt-2">
                <span className="flex-grow">{p.name}</span>
                <Money className="flex-no-shrink flex-no-grow" amount={p.price} />
                <span className="flex-no-shrink flex-no-grow">
                  <Button className="ml-2" action={() => this.startEdit(p)} disabled={!!this.state.editing}><Icon type="edit" className="w-4 h-4 fill-current nudge-d-1" /></Button>
                  <Button className="ml-2" action={() => this.delete(p)} disabled={!!this.state.editing}><Icon type="delete" className="w-4 h-4 fill-current nudge-d-1" /></Button>
                </span>
              </div>
            )) }
          </div>
        )}
      </div>
    )
  }
}  