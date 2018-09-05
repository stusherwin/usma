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
                 , form: Form.create({ code: Field.create((v: string) => v, (v: string) => v,
                                         [ Validate.required('Code is required')
                                         ])
                                     , name: Field.create((v: string) => v, (v: string) => v,
                                         [ Validate.required('Name is required')
                                         ])
                                     , price: Field.create((v: string) => Math.floor((parseFloat(v) || 0) * 100), (v: number | null) => !v? '' : (v / 100.0).toFixed(2),
                                         [ Validate.required('Price is required')
                                         , Validate.decimal('Price must be a number')
                                         , Validate.twoDP('Price can\'t have more than 2 decimal places')
                                         , Validate.greaterThanZero('Price must be more than zero')
                                         ])
                                     , vatRate: Field.create((v: string) => v as VatRate, (v: VatRate) => v,
                                         [ Validate.required('VatRate is required')
                                         ])
                                     })
                 }
  }

  startCreate = () => this.setState({ editing: 'new'
                                    , form: this.state.form.reset({code: '', name: '', price: '', vatRate: 'Zero'})
                                    })

  cancelCreate = () => this.setState({ editing: null
                                     , form: this.state.form.reset({code: '', name: '', price: '', vatRate: 'Zero'})
                                     })

  confirmCreate = () => {
    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.createProduct(validated.fields.code.value, validated.fields.name.value, validated.fields.price.value, validated.fields.vatRate.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({code: '', name: '', price: '', vatRate: 'Zero'})
                                 })
        )
    }
  }

  startEdit = (product: Product) => this.setState({ editing: product.id
                                                  , form: this.state.form.reset({ code: product.code
                                                                                , name: product.name
                                                                                , price: product.price
                                                                                , vatRate: product.vatRate
                                                                                })
                                                  })

  cancelEdit = () => {
    this.setState({ editing: null
                  , form: this.state.form.reset({code: '', name: '', price: '', vatRate: 'Zero'})
                  })
  }

  confirmEdit = () => {
    if(typeof this.state.editing !== 'number') return

    const validated = this.state.form.validate()
    this.setState({ form: validated })

    if(validated.valid()) {
      this.props.request(ServerApi.command.updateProduct(this.state.editing, validated.fields.code.value, validated.fields.name.value, validated.fields.price.value, validated.fields.vatRate.value))
        .then(this.props.reload)
        .then(_ => this.setState({ editing: null
                                 , form: this.state.form.reset({name: '', price: ''})
                                 }))
    }
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })
  
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
          <div className="bg-img-product bg-no-repeat bg-16 pl-20 min-h-16 relative mt-4">
            <h2 className="text-white leading-none mb-2 -mt-1">Products{!!this.props.loading && <Icon type="refresh" className="w-4 h-4 rotating ml-2 fill-current" />}</h2>
            <div className="flex justify-start">
              <Button action={this.startCreate} disabled={!!this.state.editing}><Icon type="add" className="w-4 h-4 mr-2 fill-current nudge-d-2" />New product</Button>
            </div>
          </div>
        </div>
        {this.state.editing == 'new' &&
          <div className="bg-product-lightest p-2">
            <h3 className="mb-4">Create new product</h3>
            <TextField id="create-code"
                       label="Code"
                       field={this.state.form.fields.code}
                       valueOnChange={this.fieldChanged('code')} />
            <TextField id="create-name"
                       label="Name"
                       field={this.state.form.fields.name}
                       valueOnChange={this.fieldChanged('name')} />
            <MoneyField id="create-price"
                        label="Price"
                        field={this.state.form.fields.price}
                        valueOnChange={this.fieldChanged('price')} />
            <DropDownField id="create-vat-rate"
                           label="VAT rate"
                           field={this.state.form.fields.vatRate}
                           valueOnChange={this.fieldChanged('vatRate')}
                           options={['Zero', 'Standard', 'Reduced']} />
            <div className="flex justify-end items-baseline">
              <Button className="ml-2" action={this.confirmCreate} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</Button>
              <Button className="ml-2" action={this.cancelCreate}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</Button>
            </div>
          </div>
        }
        {!this.props.products.length && !this.state.editing
        ? <div>No products created yet</div>
        : (
          <div>
            { this.props.products.map((p, i) => 
            this.state.editing == p.id
            ? (
              <div key={p.id} className={classNames('bg-product-lightest p-2', {'mt-2': i > 0})}>
                <h3 className="mb-4">Edit product</h3>
                <TextField id="edit-code"
                           label="Code"
                           field={this.state.form.fields.code}
                           valueOnChange={this.fieldChanged('code')} />
                <TextField id="edit-name"
                           label="Name"
                           field={this.state.form.fields.name}
                           valueOnChange={this.fieldChanged('name')} />
                <MoneyField id="edit-price"
                            label="Price"
                            field={this.state.form.fields.price}
                            valueOnChange={this.fieldChanged('price')} />
                <DropDownField id="edit-vat-rate"
                               label="VAT rate"
                               field={this.state.form.fields.vatRate}
                               valueOnChange={this.fieldChanged('vatRate')}
                               options={['Zero', 'Standard', 'Reduced']} />
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