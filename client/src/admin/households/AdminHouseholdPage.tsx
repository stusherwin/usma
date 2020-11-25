import * as React from 'react';
import { useState, useRef } from 'react'

import { Household, CollectiveOrder, ProductCatalogueEntry, GroupSettings } from 'util/Types'
import { Money, Balance } from 'util/Money'
import { Collapsible, CollapsibleState } from 'util/Collapsible'

import { CollectiveOrderDetails } from 'household/CollectiveOrderDetails'
import { PastHouseholdOrders } from 'household/PastHouseholdOrders'
import { HouseholdPayments } from 'household/HouseholdPayments'
import { EditHousehold } from 'household/EditHousehold'

import { AdminTopNav } from 'admin/AdminTopNav'

export interface AdminHouseholdPageProps {
  household: Household
  collectiveOrder: CollectiveOrder | undefined
  products: ProductCatalogueEntry[]
  households: Household[]
  categories: string[]
  brands: string[]
  groupSettings: GroupSettings
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export const AdminHouseholdPage = (props: AdminHouseholdPageProps) => {
  const editHousehold: React.RefObject<EditHousehold> = useRef(null)
  const [collapsibleState, setCollapsibleState] = useState(new CollapsibleState('orders', s => setCollapsibleState(s)))

  return (
    <div className="bg-household-light min-h-screen">
      <AdminTopNav />
      <Collapsible
        collapsibleKey="household"
        collapsibleState={collapsibleState}
        onExpand={() => { if (editHousehold.current) { editHousehold.current.reset() } }}
        onCollapse={() => { if (editHousehold.current) { editHousehold.current.blur() } }}
        onExpanded={() => { if (editHousehold.current) { editHousehold.current.focus() } }}
        header={
          <div className="p-2 pt-0 pb-4 bg-household-light min-h-28">
            <svg className="w-16 h-16 absolute">
              <use xlinkHref="#icon-household" />
            </svg>
            <div className="ml-20 mt-2 flex items-start items-baseline justify-between">
              <h2 className="mt-1 leading-none mr-2 mb-4">
                {props.household.name}
              </h2>
              {props.groupSettings.enablePayments &&
                <Balance className="py-1 px-1 -mr-1 ml-auto" amount={-props.household.balance} />
              }
            </div>
            <div className="ml-20 text-base"><strong>Contact:</strong> {props.household.contactName || 'none'}</div>
          </div>
        }>
        <EditHousehold
          ref={editHousehold}
          onConfirm={() => props.reload().then(collapsibleState.toggle('household'))}
          onCancel={collapsibleState.toggle('household')}
          {...props} />
      </Collapsible>
      <CollectiveOrderDetails
        collapsibleKey="orders"
        collapsibleState={collapsibleState}
        {...props} />
      <PastHouseholdOrders
        collapsibleKey="past-orders"
        collapsibleState={collapsibleState}
        {...props} />
      {props.groupSettings.enablePayments &&
        <HouseholdPayments
          collapsibleKey="payments"
          collapsibleState={collapsibleState}
          {...props} />
      }
      {props.groupSettings.enablePayments &&
        <div className="p-2 pl-20 text-black relative mt-2">
          <h3 className="mt-0 ml-2 flex justify-between">
            <span className="border-t-2 border-b-2 border-household-light pt-1 pb-1">Balance:</span>
            <Money className="text-right border-t-2 border-b-2 border-black pt-1 pb-1" amount={-props.household.balance} noColour />
          </h3>
        </div>
      }
    </div>
  )
}