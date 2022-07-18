import * as React from 'react'
import * as classNames from 'classnames'

import { CollectiveOrder, Household, GroupSettings, ProductCatalogueEntry } from 'util/Types'
import { Collapsible, CollapsibleState } from 'util/Collapsible'
import { ServerApi } from 'util/ServerApi'
import { Icon } from 'util/Icon'

import { OrderTabs } from 'order/OrderTabs'
import { OrderItems } from 'order/OrderItems'
import { OrderTotal } from 'order/OrderTotal'
import { OrderStatus } from 'order/OrderStatus'
import { OrderMessages, getMessages } from 'order/OrderMessages'

import { AdminTopNav } from 'admin/AdminTopNav'

import { PastCollectiveOrders } from './PastCollectiveOrders'
import { HouseholdOrders } from './HouseholdOrders'
import { ProductCodes } from './ProductCodes'
import { CollectiveOrderButtons } from './CollectiveOrderButtons'
import { ReconcileOrder } from './ReconcileOrder'
import { ReconcileOrderFileUpload } from './ReconcileOrderFileUpload'

export interface AdminOrdersPageProps {
  collectiveOrder: CollectiveOrder | undefined
  pastOrders: CollectiveOrder[]
  households: Household[]
  products: ProductCatalogueEntry[]
  categories: string[]
  brands: string[]
  groupSettings: GroupSettings
  request: <T extends {}>(p: Promise<T>) => Promise<T>
  reload: () => Promise<void>
  showProductImage: (productCode: string) => void
}

export interface AdminOrdersPageState {
  collapsibleState: CollapsibleState
  addingHousehold: Household | undefined
  tab: 'households' | 'product-list' | 'product-codes'
  reconcilingOrder: boolean
  uploadingOrderId: number | undefined
}

export class AdminOrdersPage extends React.Component<AdminOrdersPageProps, AdminOrdersPageState> {
  constructor(props: AdminOrdersPageProps) {
    super(props)

    this.state = {
      collapsibleState: new CollapsibleState('order', collapsibleState => this.setState({ collapsibleState })),
      addingHousehold: undefined,
      tab: 'households',
      reconcilingOrder: false,
      uploadingOrderId: undefined,
    }
  }

  newOrder = () => {
    this.props.request(ServerApi.command.createOrder())
      .then(this.props.reload)
  }

  abandonOrder = () => {
    if (!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.abandonOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  placeOrder = () => {
    if (!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.placeOrder(this.props.collectiveOrder.id))
      .then(this.props.reload)
  }

  startReconcilingOrder = () => {
    if (!this.props.collectiveOrder) return

    this.setState({ reconcilingOrder: true })
  }

  endReconcilingOrder = () => {
    if (!this.state.reconcilingOrder) return

    this.setState({ reconcilingOrder: false })
  }

  endReconcilingItem = (productId: number, productPriceExcVat: number, households: { householdId: number, itemQuantity: number }[]) => {
    if (!this.state.reconcilingOrder) return
    if (!this.props.collectiveOrder) return

    this.props.request(ServerApi.command.reconcileOrderItem(this.props.collectiveOrder.id, productId, productPriceExcVat, households))
      .then(this.props.reload)
  }

  startUpload = (orderId: number) => {
    this.setState({
      uploadingOrderId: orderId
    })
  }

  cancelUpload = () => {
    this.setState({
      uploadingOrderId: undefined
    })
  }

  render() {
    const order = this.props.collectiveOrder
    const messages = getMessages(order)

    return (
      <div className="bg-order-dark min-h-screen">
        <AdminTopNav />
        <Collapsible
          collapsibleKey="order"
          collapsibleState={this.state.collapsibleState}
          onCollapsed={this.endReconcilingOrder}
          {...this.props}
          header={
            <div className="p-2 pt-4 bg-order-dark min-h-24">
              <svg className="w-16 h-16 absolute">
                <use xlinkHref="#icon-order" />
              </svg>
              <div className="flex items-baseline justify-between ml-20">
                <div>
                  <h2 className="leading-none">
                    Current order
                             </h2>
                  <h3 className="mt-4">
                    <OrderStatus order={order} />
                  </h3>
                </div>
                <h3 className="ml-2">
                  <OrderTotal order={order} />
                </h3>
              </div>
            </div>
          }
          expandedHeader={
            <div className="p-2 bg-order-dark -mt-4">
              {!this.state.reconcilingOrder && (!order || this.state.uploadingOrderId != order.id) &&
                <CollectiveOrderButtons
                  order={order}
                  newOrder={this.newOrder}
                  abandonOrder={this.abandonOrder}
                  placeOrder={this.placeOrder}
                  reconcileOrder={this.startReconcilingOrder}
                  uploadOrderFile={order && (() => this.startUpload(order.id)) || undefined} />
              }
              {!this.state.reconcilingOrder && !!order && !!order.items.length && this.state.uploadingOrderId != order.id &&
                <div className="mt-5">
                  <OrderTabs
                    tab={this.state.tab}
                    setTab={tab => this.setState({ tab })}
                    householdsBg={messages.length ? 'bg-blue-lighter' : 'bg-household-lightest'}
                    productsBg={messages.length ? 'bg-blue-lighter' : 'bg-white'}
                    productCodesBg={messages.length ? 'bg-blue-lighter' : 'bg-white'} />
                </div>
              }
            </div>
          }>
          {!!order && (
            this.state.reconcilingOrder ?
              <ReconcileOrder
                order={order}
                endReconcilingOrder={this.endReconcilingOrder}
                endReconcilingItem={this.endReconcilingItem}
                {...this.props} />
              : !order.items.length ?
                <div className="shadow-inner-top border-t bg-white px-2 py-4 text-black">
                  <Icon type="info" className="w-4 h-4 mr-2 fill-current nudge-d-2" />No order items
              </div>
                : this.state.uploadingOrderId == order.id ?
                  <ReconcileOrderFileUpload
                    collectiveOrder={order}
                    bgColor="bg-household-lightest"
                    cancelUpload={this.cancelUpload}
                    request={this.props.request}
                    reload={this.props.reload} />
                  : this.state.tab == 'households' ?
                    <div className={classNames("border-t bg-household-lightest", { "shadow-inner-top": this.state.uploadingOrderId != order.id })}>
                      <OrderMessages order={order} />
                      <div className="flex justify-end mt-4 mr-2">
                        <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url.householdOrdersDownload(order)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                      </div>
                      <HouseholdOrders order={order}
                        {...this.props} />
                    </div>
                    : this.state.tab == 'product-list' ?
                      <div className={classNames("border-t bg-white", { "shadow-inner-top": this.state.uploadingOrderId != order.id })}>
                        <OrderMessages order={order} />
                        <div className="flex justify-end mr-2 mt-4 mb-2">
                          <button className="flex-no-grow flex-no-shrink" onClick={() => document.location.href = ServerApi.url.collectiveOrderDownload(order)}><Icon type="download" className="w-4 h-4 fill-current mr-2 nudge-d-2" />Download CSV file</button>
                        </div>
                        <OrderItems order={order}
                          {...this.props} />
                      </div>
                      : <div className={classNames("border-t border-t bg-white", { "shadow-inner-top": this.state.uploadingOrderId != order.id })}>
                        <OrderMessages order={order} />
                        <ProductCodes order={order} />
                      </div>
          )
          }
        </Collapsible>
        <PastCollectiveOrders
          collapsibleKey="past-orders"
          collapsibleState={this.state.collapsibleState}
          uploadingOrderId={this.state.uploadingOrderId}
          startUpload={this.startUpload}
          cancelUpload={this.cancelUpload}
          {...this.props} />
      </div>
    )
  }
}