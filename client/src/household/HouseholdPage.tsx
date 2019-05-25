import * as React from 'react';
import * as classNames from 'classnames'

import { Household, HouseholdOrder, PastHouseholdOrder, CollectiveOrder, HouseholdPayment, ProductCatalogueEntry } from '../Types'
import { Icon } from '../common/Icon'
import { Money } from '../common/Money'
import { Router } from '../common/Router'
import { CurrentOrder } from './CurrentOrder'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'
import { ServerApi, ApiError } from '../ServerApi'
import { Form, Field, Validate } from '../common/Validation'
import { TextField } from '../common/Field'
import { CollapsibleWithHeader } from './CollapsibleWithHeader'
import { RouterLink } from '../common/RouterLink';

export interface HouseholdOrdersPageProps { household: Household
                                          , currentOrder: CollectiveOrder | null
                                          , currentHouseholdOrder: HouseholdOrder | null
                                          , currentHouseholdOrders: HouseholdOrder[]
                                          , pastHouseholdOrders: PastHouseholdOrder[]
                                          , payments: HouseholdPayment[]
                                          , products: ProductCatalogueEntry[]
                                          , households: Household[]
                                          , request: <T extends {}>(p: Promise<T>) => Promise<T>
                                          , reload: () => Promise<void>
                                          , loading: boolean
                                          , error: ApiError | null
                                          , router: Router
                                          }
type Section = 'orders' | 'past-orders' | 'payments' | 'household'
export interface HouseholdOrdersPageState { expanded: Section | null
                                          , form: Form
                                          , editFocused: boolean
                                          }
export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, HouseholdOrdersPageState> {  
  nameInput: React.RefObject<HTMLInputElement>

  constructor(props: HouseholdOrdersPageProps) {
    super(props)
    this.state = { 
      expanded: 'orders', 
      editFocused: false,
      form: Form.create({ 
        name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]),
        contactName: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactEmail: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactPhone: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', [])
      })
    }
    this.nameInput = React.createRef();
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand}));
  }

  confirmEdit = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.props.household.id, validated.fields.name.value, validated.fields.contactName.value, validated.fields.contactEmail.value, validated.fields.contactPhone.value))
        .then(this.props.reload)
        .then(_ => this.toggle('household')())
    }
  }

  cancelEdit = () => {
    this.toggle('household')()
    this.setState({ form: this.state.form.reset({ name: this.props.household.name
                                                , contactName: this.props.household.contactName
                                                , contactEmail: this.props.household.contactEmail
                                                , contactPhone: this.props.household.contactPhone
                                                })
                  })
  }

  populateFields = () => { 
    this.setState({ form: this.state.form.reset({ name: this.props.household.name
                                                                     , contactName: this.props.household.contactName
                                                                     , contactEmail: this.props.household.contactEmail
                                                                     , contactPhone: this.props.household.contactPhone
                                                                     })
                                       })
  }

  fieldChanged = (fieldName: string) => (value: string) => {
    this.setState({ form: this.state.form.update(fieldName, value) })
  }

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <CollapsibleWithHeader className="min-h-28"
                               headerClassName="bg-household-light min-h-28"
                               headingClassName="mt-2"
                               headerImageClassName="bg-img-household mt-2"
                               headerText={this.props.household.name}
                               headerContent={() => (
                                 <div>
                                   <div className="ml-20 mt-1">
                                     <RouterLink path="/households">Change household</RouterLink>
                                   </div>
                                   <div className="ml-20 text-lg mt-4"><strong>Contact:</strong> {this.props.household.contactName || 'none'}</div>
                                   {/* <table className="border-collapse w-full mt-1">
                                     {this.props.household.contactName &&
                                       <tr>
                                         <th className="font-bold text-left pt-1 pl-20 pr-2">Contact:</th>
                                         <td className="pt-1">{this.props.household.contactName}</td>
                                       </tr>
                                     } */}
                                     {/* {this.props.household.contactEmail &&
                                       <tr>
                                         <th className="font-bold text-left pt-1 pr-2">Email:</th>
                                         <td className="pt-1">{this.props.household.contactEmail}</td>
                                       </tr>
                                     }
                                     {this.props.household.contactPhone &&
                                       <tr>
                                         <th className="font-bold text-left pt-1 pr-2">Phone:</th>
                                         <td className="pt-1">{this.props.household.contactPhone}</td>
                                       </tr>
                                     } */}
                                   {/* </table> */}
                                 </div>
                               )}
                               expanded={this.state.expanded == 'household'}
                               otherExpanding={!!this.state.expanded && this.state.expanded != 'household'}
                               toggle={this.toggle('household')}
                               onExpand={this.populateFields}
                               onCollapse={() => { if(this.nameInput.current) { this.nameInput.current.blur() } }}
                               onExpanded={() => { if(this.nameInput.current) { this.nameInput.current.focus() } }}>
          <div className="shadow-inner-top px-2 py-4 bg-household-lightest">
            <h3 className="mb-4">Edit household</h3>
            <TextField id="edit-name"
                       label="Name"
                       inputRef={this.nameInput}
                       field={this.state.form.fields.name}
                       valueOnChange={this.fieldChanged('name')} />
            <TextField id="edit-contactName"
                       label="Contact name"
                       field={this.state.form.fields.contactName}
                       valueOnChange={this.fieldChanged('contactName')} />
            <TextField id="edit-contactEmail"
                       label="Contact email"
                       field={this.state.form.fields.contactEmail}
                       valueOnChange={this.fieldChanged('contactEmail')} />
            <TextField id="edit-contactPhone"
                       label="Contact phone"
                       field={this.state.form.fields.contactPhone}
                       valueOnChange={this.fieldChanged('contactPhone')} />
            <div className="flex justify-end">
              <button className="ml-2" onClick={this.confirmEdit} disabled={!this.state.form.valid()}><Icon type="ok" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Save</button>
              <button className="ml-2" onClick={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button>
            </div>
          </div>
        </CollapsibleWithHeader>
        <CurrentOrder household={this.props.household}
                      currentOrder={this.props.currentOrder}
                      currentHouseholdOrder={this.props.currentHouseholdOrder}
                      currentHouseholdOrders={this.props.currentHouseholdOrders}
                      products={this.props.products}
                      households={this.props.households}
                      loading={this.props.loading}
                      expanded={this.state.expanded == 'orders'}
                      otherExpanding={!!this.state.expanded && this.state.expanded != 'orders'}
                      toggle={this.toggle('orders')}
                      request={this.props.request}
                      reload={this.props.reload} />
        <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                             expanded={this.state.expanded == 'past-orders'}
                             otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders'}
                             toggle={this.toggle('past-orders')}
                             request={this.props.request}
                             reload={this.props.reload} />
        <HouseholdPayments household={this.props.household}
                           payments={this.props.payments}
                           expanded={this.state.expanded == 'payments'}
                           otherExpanding={!!this.state.expanded && this.state.expanded != 'payments'}
                           toggle={this.toggle('payments')}
                           request={this.props.request}
                           reload={this.props.reload} />
        <div className="bg-household-light p-2 pl-20 text-black">
          <h3 className="mt-0 ml-2 flex justify-between"><span>Balance:</span><span><Money amount={this.props.household.balance} /></span></h3>
        </div>
        <div className={classNames('fixed pin bg-black flex items-center justify-center text-grey-lighter', {
            'pointer-events-none': !this.props.loading
          })} style={{
            background: 'rgba(0,0,0,0.3)',
            opacity: this.props.loading? 1 : 0,
            transition: 'opacity 0.25s ease'
          }}>
          <Icon type="loading" className="w-16 h-16 -mt-4 rotating fill-current" />
        </div>
      </div>
    )
  }
}