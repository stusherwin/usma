import * as React from 'react';

import { Household, CollectiveOrder, ProductCatalogueEntry, GroupSettings } from 'util/Types'
import { Money, Balance } from 'util/Money'
import { Router } from 'util/Router'
import { Icon } from 'util/Icon'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { RouterLink } from 'util/RouterLink'

import { CollectiveOrderDetails } from './CollectiveOrderDetails'
import { PastHouseholdOrders } from './PastHouseholdOrders'
import { HouseholdPayments } from './HouseholdPayments'
import { EditHousehold } from './EditHousehold'

export interface HouseholdPageProps {
  household: Household
  collectiveOrder: CollectiveOrder | undefined
  products: ProductCatalogueEntry[]
  categories: string[]
  brands: string[]
  households: Household[]
  groupSettings: GroupSettings
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  router: Router
  showProductImage: (productCode: string) => void
}

export interface HouseholdPageState {
  collapsibleState: CollapsibleState
}

export class HouseholdPage extends React.Component<HouseholdPageProps, HouseholdPageState> {
  editHousehold: React.RefObject<EditHousehold>

  constructor(props: HouseholdPageProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState('orders', collapsibleState => this.setState({ collapsibleState }))
    }

    this.editHousehold = React.createRef();
  }

  render() {
    return (
      <div className="bg-household-light min-h-screen">
        <RouterLink className="block p-2 text-black no-underline hover:text-black hover:underline" path="/households"><Icon type="left-arrow" className="w-3 h-3 fill-current mr-1" />Change household</RouterLink>
        <Collapsible
          collapsibleKey="household"
          collapsibleState={this.state.collapsibleState}
          onExpand={() => { if (this.editHousehold.current) { this.editHousehold.current.reset() } }}
          onCollapse={() => { if (this.editHousehold.current) { this.editHousehold.current.blur() } }}
          onExpanded={() => { if (this.editHousehold.current) { this.editHousehold.current.focus() } }}
          header={
            <div className="p-2 pt-0 pb-4 bg-household-light min-h-28">
              <div className="bg-no-repeat w-16 h-16 absolute bg-img-household"></div>
              <div className="ml-20 mt-2 flex items-start items-baseline justify-between">
                <h2 className="mt-1 leading-none mr-2 mb-4">
                  {this.props.household.name}
                </h2>
                {this.props.groupSettings.enablePayments &&
                  <Balance className="py-1 px-1 -mr-1 ml-auto" amount={-this.props.household.balance} />
                }
              </div>
              <div className="ml-20 text-base"><strong>Contact:</strong> {this.props.household.contactName || 'none'}</div>
            </div>
          }>
          <EditHousehold
            ref={this.editHousehold}
            {...this.props}
            onConfirm={() => this.props.reload().then(this.state.collapsibleState.toggle('household'))}
            onCancel={this.state.collapsibleState.toggle('household')} />
        </Collapsible>
        <CollectiveOrderDetails
          collapsibleKey="orders"
          collapsibleState={this.state.collapsibleState}
          {...this.props} />
        <PastHouseholdOrders
          collapsibleKey="past-orders"
          collapsibleState={this.state.collapsibleState}
          {...this.props} />
        {this.props.groupSettings.enablePayments &&
          <HouseholdPayments
            collapsibleKey="payments"
            collapsibleState={this.state.collapsibleState}
            readOnly={true}
            {...this.props} />
        }
        {this.props.groupSettings.enablePayments &&
          <div className="p-2 pl-20 text-black relative mt-2">
            <h3 className="mt-0 ml-2 flex justify-between">
              <span className="border-t-2 border-b-2 border-household-light pt-1 pb-1">Balance:</span>
              <Money className="text-right border-t-2 border-b-2 border-black pt-1 pb-1" amount={-this.props.household.balance} noColour />
            </h3>
          </div>
        }
      </div>
    )
  }
}