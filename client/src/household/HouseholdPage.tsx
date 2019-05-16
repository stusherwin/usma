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

const transitionTime = 0.25;
const minHeight = '5rem';

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
type Section = 'orders' | 'past-orders' | 'payments'
export interface HouseholdOrdersPageState { expanded: Section | null
                                          , thisExpanded: boolean 
                                          , form: Form
                                          }
export class HouseholdPage extends React.Component<HouseholdOrdersPageProps, HouseholdOrdersPageState> {  
  container: React.RefObject<HTMLDivElement>

  constructor(props: HouseholdOrdersPageProps) {
    super(props)
    this.state = { 
      expanded: 'orders', 
      thisExpanded: false,
      form: Form.create({ 
        name: Field.create((v: string) => v, (v: string) => v, [Validate.required('Name is required')]),
        contactName: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactEmail: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', []),
        contactPhone: Field.create((v: string) => v.replace(/\s+/, '').length? v : null, (v: string | null) => v || '', [])
      })
    }

    this.container = React.createRef();
  }

  componentDidMount() {
    this.animateHeight()
  }

  animateHeight() {
    const el = this.container.current
    if(!el) return

    if(this.state.thisExpanded) {
      el.style.height = el.scrollHeight + 'px';
    } else {
      el.style.height = el.scrollHeight + 'px';
      el.offsetHeight; // trigger reflow
      el.style.height = minHeight;
    }
  }

  unsetHeight = () => {
    const el = this.container.current
    if(!el) return

    if(this.state.thisExpanded) {
      el.style.height = null;
    }
  }

  toggle = (toExpand: Section) => () => { 
    this.setState(({expanded}) => ({expanded: toExpand == expanded? null : toExpand, thisExpanded: false}));
    setTimeout(() => {
      this.animateHeight();
    })
  }

  toggleThisExpanded = () => {
    this.setState(({thisExpanded}) => ({ thisExpanded: !thisExpanded, expanded: null }));
    setTimeout(() => {
      this.animateHeight();
    })
  }

  confirmEdit = () => {
    const validated = this.state.form.validate()
    console.log(validated)
    this.setState({ form: validated })
    if(validated.valid()) {
      this.props.request(ServerApi.command.updateHousehold(this.props.household.id, validated.fields.name.value, validated.fields.contactName.value, validated.fields.contactEmail.value, validated.fields.contactPhone.value))
        .then(this.props.reload)
        .then(_ => this.setState({ form: this.state.form.reset({name: '', contactName: null, contactEmail: null, contactPhone: null})
                                 })
        )
    }
  }

  fieldChanged = (fieldName: string) => (value: string) =>
    this.setState({ form: this.state.form.update(fieldName, value) })

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        {!!this.props.error && (
          <div>{this.props.error.error}: {this.props.error.message}</div>
        )}
        <div ref={this.container} className="min-h-32" style={{ 
            overflow: 'hidden',
            height: minHeight,
            transition: `height ${transitionTime / 2}s ease`,
            transitionDelay: this.state.thisExpanded? '0s' : (!!this.state.expanded? `${transitionTime / 2}s` : '0s')
          }} onTransitionEnd={this.unsetHeight}>
          <a href="#" onClick={e => { e.preventDefault(); this.toggleThisExpanded() }} className={classNames(
            'bg-household-light p-2 pt-4 pb-4 block no-underline hover:no-underline text-black hover:text-black min-h-20', {
            })}>
            <div className="bg-img-household bg-no-repeat w-16 h-16 absolute"></div>
            <h2 className="leading-none ml-20 relative flex">{this.props.household.name}
              <Icon type={this.state.thisExpanded? 'collapse' : 'expand'} className="w-4 h-4 fill-current absolute pin-r mt-1" />
            </h2>
            <table className="border-collapse w-full mt-1 ml-20">
              {this.props.household.contactName &&
                <tr>
                  <th className="font-bold text-left pt-1 pr-2">Contact:</th>
                  <td className="pt-1">{this.props.household.contactName}</td>
                </tr>
              }
              {this.props.household.contactEmail &&
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
              }
            </table>
          </a>
          <div className="shadow-inner-top bg-white px-2 py-4">
            <div className={classNames('bg-household-lightest p-2')}>
              <h3 className="mb-4">Edit household</h3>
              <TextField id="edit-name"
                         label="Name"
                         autofocus
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
                {/* <button className="ml-2" onClick={this.cancelEdit}><Icon type="cancel" className="w-4 h-4 mr-2 fill-current nudge-d-1" />Cancel</button> */}
              </div>
            </div>
          </div>
        </div>
        <CurrentOrder household={this.props.household}
                      currentOrder={this.props.currentOrder}
                      currentHouseholdOrder={this.props.currentHouseholdOrder}
                      currentHouseholdOrders={this.props.currentHouseholdOrders}
                      products={this.props.products}
                      households={this.props.households}
                      loading={this.props.loading}
                      expanded={this.state.expanded == 'orders'}
                      otherExpanding={!!this.state.expanded && this.state.expanded != 'orders' || this.state.thisExpanded}
                      toggle={this.toggle('orders')}
                      request={this.props.request}
                      reload={this.props.reload} />
        <PastHouseholdOrders householdOrders={this.props.pastHouseholdOrders}
                             expanded={this.state.expanded == 'past-orders'}
                             otherExpanding={!!this.state.expanded && this.state.expanded != 'past-orders' || this.state.thisExpanded}
                             toggle={this.toggle('past-orders')}
                             request={this.props.request}
                             reload={this.props.reload} />
        <HouseholdPayments household={this.props.household}
                           payments={this.props.payments}
                           expanded={this.state.expanded == 'payments'}
                           otherExpanding={!!this.state.expanded && this.state.expanded != 'payments' || this.state.thisExpanded}
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