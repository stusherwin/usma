import { Household, CollectiveOrder, HouseholdOrder, HouseholdPayment, ProductCatalogueEntry, GroupSettings } from './Types'
import { Util } from './Util'

export interface Data { collectiveOrders: CollectiveOrder[]
                      , households: Household[]
                      , groupSettings: GroupSettings
                      }

export interface ApiData { collectiveOrder: CollectiveOrder | null
                         , pastCollectiveOrders: CollectiveOrder[]
                         , householdOrders: HouseholdOrder[]
                         , pastHouseholdOrders: HouseholdOrder[]
                         , households: Household[]
                         , householdPayments: HouseholdPayment[]
                         , groupSettings: GroupSettings
                         }

export interface CatalogueData { productCatalogue: ProductCatalogueEntry[]
                               , categories: string[]
                               , brands: string[]
                               }
function getData(): Promise<ApiData> {
  return Http.get<ApiData>(groupUrl('/query/data'))
    .then(res => { 
      if(res) {
        if(res.collectiveOrder) {
          res.collectiveOrder.orderCreatedDate = new Date(res.collectiveOrder.orderCreatedDate)
        }
        res.pastCollectiveOrders.forEach(o => o.orderCreatedDate = new Date(o.orderCreatedDate))
        res.householdOrders.forEach(ho => { ho.orderCreatedDate = new Date(ho.orderCreatedDate); })
        res.pastHouseholdOrders.forEach(ho => ho.orderCreatedDate = new Date(ho.orderCreatedDate))
        res.households.forEach(h => { })
        res.householdPayments.forEach(hp => { hp.date = new Date(hp.date);})
      }
      return res;
    })
}

function getProductCatalogueData(): Promise<CatalogueData> {
  return Http.get<CatalogueData>(groupUrl('/query/product-catalogue-data'))
}

const query = {
  getCatalogueData(): Promise<CatalogueData> {
    return getProductCatalogueData()
  },

  getData(): Promise<Data> {
    return getData()
      .then(({collectiveOrder, pastCollectiveOrders, householdOrders, pastHouseholdOrders, households, householdPayments, groupSettings}) => {
        if(collectiveOrder) {
          collectiveOrder.householdOrders = householdOrders.filter(ho => ho.orderId == collectiveOrder.id)
        }

        for(let po of pastCollectiveOrders) {
          po.householdOrders = pastHouseholdOrders.filter(ho => ho.orderId == po.id)
        }

        let collectiveOrders = pastCollectiveOrders
        if(collectiveOrder) {
          collectiveOrders.unshift(collectiveOrder)
        }

        let ho = householdOrders.concat(pastHouseholdOrders)

        for(let h of households) {
          h.currentHouseholdOrder = collectiveOrders[0] && (ho.filter(ho => ho.householdId == h.id && ho.orderId == collectiveOrders[0].id)[0])
          h.pastHouseholdOrders = pastHouseholdOrders.filter(pho => pho.householdId == h.id && (!h.currentHouseholdOrder || pho.orderId != h.currentHouseholdOrder.orderId))
          h.householdPayments = householdPayments.filter(p => p.householdId == h.id)
        }

        return {
          collectiveOrders,
          households,
          groupSettings
        }                       
      })
  }
}

const command = {
  createOrder(householdId?: number): Promise<number> {
    return Http.post(groupUrl(`/command/create-order/${householdId || ''}`), {})
  },

  placeOrder(orderId: number): Promise<number> {
    return Http.post(groupUrl(`/command/place-order/${orderId}`), {})
  },

  abandonOrder(orderId: number): Promise<number> {
    return Http.post(groupUrl(`/command/abandon-order/${orderId}`), {})
  },

  abandonHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/abandon-household-order/${orderId}/${householdId}`), {})
  },

  completeHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/complete-household-order/${orderId}/${householdId}`), {})
  },

  reopenHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/reopen-household-order/${orderId}/${householdId}`), {})
  },

  ensureHouseholdOrderItem(orderId: number, householdId: number, productCode: string, quantity: number | null): Promise<{}> {
    return Http.post(groupUrl(`/command/ensure-household-order-item/${orderId}/${householdId}/${productCode}`), { hoidQuantity: quantity })
  },

  ensureAllItemsFromPastHouseholdOrder(orderId: number, householdId: number, pastOrderId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/ensure-all-past-order-items/${orderId}/${householdId}/${pastOrderId}`), {})
  },

  removeHouseholdOrderItem(orderId: number, householdId: number, productId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/remove-household-order-item/${orderId}/${householdId}/${productId}`), {})
  },

  createHousehold(name: string, contactName: string | null, contactEmail: string | null, contactPhone: string | null): Promise<number> {
    return Http.post(groupUrl(`/command/create-household/`), { hdName: name
                                                             , hdContactName: contactName
                                                             , hdContactEmail: contactEmail
                                                             , hdContactPhone: contactPhone 
                                                             })
  },

  updateHousehold(id: number, name: string, contactName: string | null, contactEmail: string | null, contactPhone: string | null): Promise<number> {
    return Http.post(groupUrl(`/command/update-household/${id}`), { hdName: name
                                                                  , hdContactName: contactName
                                                                  , hdContactEmail: contactEmail
                                                                  , hdContactPhone: contactPhone 
                                                                  })
  },

  archiveHousehold(id: number): Promise<{}> {
    return Http.post(groupUrl(`/command/archive-household/${id}`), {})
  },

  createHouseholdPayment(householdId: number, date: Date, amount: number): Promise<number> {
    return Http.post(groupUrl(`/command/create-household-payment/${householdId}`), { hpdDate: Util.dateString(date)
                                                                                   , hpdAmount: amount
                                                                                   })
  },

  updateHouseholdPayment(id: number, date: Date, amount: number): Promise<number> {
    return Http.post(groupUrl(`/command/update-household-payment/${id}`), { hpdDate: Util.dateString(date)
                                                                          , hpdAmount: amount
                                                                          })
  },

  archiveHouseholdPayment(id: number): Promise<{}> {
    return Http.post(groupUrl(`/command/archive-household-payment/${id}`), {})
  },

  uploadProductCatalogue(data: FormData): Promise<{}> {
    return Http.postFormData(groupUrl(`/command/upload-product-catalogue/`), data)
  },

  acceptCatalogueUpdates(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/accept-catalogue-updates/${orderId}/${householdId}`), {})
  },

  reconcileOrderItem(orderId: number, productId: number, productPriceExcVat: number, householdQuantities: {householdId: number, itemQuantity: number}[]): Promise<{}> {
    return Http.post(groupUrl(`/command/reconcile-order-item/${orderId}/${productId}`), 
      { roidProductPriceExcVat: productPriceExcVat
      , roidHouseholdQuantities: householdQuantities.map(h => ({ hqdHouseholdId: h.householdId, hqdItemQuantity: h.itemQuantity }))
      })
  },

  uploadReconcileHouseholdOrder(data: FormData): Promise<{}> {
    return Http.postFormData(groupUrl(`/command/upload-reconcile-household-order/`), data)
  },
}

const groupUrl = (url: string) => {
  const groupKey = window.location.href.split('/').filter(l => l.length).slice(3, 4).join('/')
  return `/api/${groupKey}${url}`
}

export const ServerApi = {
  query,
  command,
  url: (url: string) => groupUrl(`/${url}`),
  verifyGroup(groupKey: string | null): Promise<boolean> {
    return Http.post(`/api/verify/${groupKey}`, {})
  }
}

export class Http {
  static get<T>(url: string): Promise<T> {
    return this.fetchHttpRequest(new Request(url))
  }
  
  static post<T>(url: string, body: {}): Promise<T> {
    return this.fetchHttpRequest(new Request(url,
      { method: 'POST'
      , headers: new Headers({'Content-Type' : 'application/json'})
      , body: JSON.stringify(body)
      }))
  }

  static postFormData<T>(url: string, data: FormData): Promise<T> {
    return this.fetchHttpRequest(new Request(url,
      { method: 'POST'
      , body: data
      }))
  }

  private static fetchHttpRequest<T>(req: Request): Promise<T> {
    try {
      return fetch(req, {credentials: 'same-origin'})
        .then(res => {
          if(!res.ok) {
            return res.text().then(txt => { throw new ApiError(`${res.statusText} (${res.status})`, txt, res.status) })
          }
          const contentType = res.headers.get('content-type')
          if (contentType == null || !contentType.includes('application/json')) {
            throw new ApiError('Invalid server response', 'Expected response to have content-type application/json.', null);
          }
          return res.json()
        })
        .then(res => res as T)
        .catch(err => Promise.reject(new ApiError('Error from the server', 'Received an unexpected response from the server: ' + err.error, err.status || null)))
    } catch(TypeError) {
      return Promise.reject(new ApiError('Can\'t connect to the server', 'The server seems to be down or busy, please wait a while and try again.', null))
    }
  }
}

export class ApiError {
  constructor(error: string, message: string, status: number | null) {
    this.error = error
    this.message = message
    this.status = status
  }

  error: string
  message: string
  status: number | null
}