import { Household, CollectiveOrder, PastCollectiveOrder, HouseholdOrder, PastHouseholdOrder, HouseholdPayment, ProductCatalogueEntry } from './Types'
import { Util } from './Util'

export interface Data { collectiveOrder: CollectiveOrder | null
                      , pastCollectiveOrders: PastCollectiveOrder[]
                      , householdOrders: HouseholdOrder[]
                      , pastHouseholdOrders: PastHouseholdOrder[]
                      , productCatalogue: ProductCatalogueEntry[]
                      , categories: string[]
                      , brands: string[]
                      , households: Household[]
                      , householdPayments: HouseholdPayment[]
}

function getCollectiveOrder(): Promise<CollectiveOrder | null> {
  return Http.get<CollectiveOrder | null>(groupUrl('/query/collective-order'))
    .then(res => { 
      if(res) {
        res.createdDate = new Date(res.createdDate)
      }
      return res;
    })
}

function getPastCollectiveOrders(): Promise<PastCollectiveOrder[]> {
  return Http.get<PastCollectiveOrder[]>(groupUrl('/query/past-collective-orders'))
    .then(res => { res.forEach(o => o.createdDate = new Date(o.createdDate)); return res })
}

function getHouseholdOrders(): Promise<HouseholdOrder[]> {
  return Http.get<HouseholdOrder[]>(groupUrl('/query/household-orders'))
    .then(res => { res.forEach(ho => { ho.orderCreatedDate = new Date(ho.orderCreatedDate); }); return res })
}

function getPastHouseholdOrders(): Promise<PastHouseholdOrder[]> {
  return Http.get<PastHouseholdOrder[]>(groupUrl('/query/past-household-orders'))
    .then(res => { res.forEach(ho => ho.orderCreatedDate = new Date(ho.orderCreatedDate)); return res })
}

function getHouseholds(): Promise<Household[]> {
  return Http.get<Household[]>(groupUrl('/query/households'))
    .then(res => { res.forEach(h => { }); return res })
}

function getHouseholdPayments(): Promise<HouseholdPayment[]> {
  return Http.get<HouseholdPayment[]>(groupUrl('/query/household-payments'))
    .then(res => { res.forEach(hp => { hp.date = new Date(hp.date);}); return res })
}

function getProductCatalogue(): Promise<ProductCatalogueEntry[]> {
  return Http.get<ProductCatalogueEntry[]>(groupUrl('/query/product-catalogue'))
}

function getProductCatalogueCategories(): Promise<string[]> {
  return Http.get<string[]>(groupUrl('/query/product-catalogue-categories'))
}

function getProductCatalogueBrands(): Promise<string[]> {
  return Http.get<string[]>(groupUrl('/query/product-catalogue-brands'))
}

const query = {
  getData(): Promise<Data> {
    return Promise.all([ getCollectiveOrder()
                       , getPastCollectiveOrders()
                       , getHouseholdOrders()
                       , getPastHouseholdOrders()
                       , getHouseholds()
                       , getHouseholdPayments()
                       , getProductCatalogue()
                       , getProductCatalogueCategories()
                       , getProductCatalogueBrands()
                       ])
      .then(([collectiveOrder, pastCollectiveOrders, householdOrders, pastHouseholdOrders, households, householdPayments, productCatalogue, categories, brands]) => {
        if(collectiveOrder) {
          collectiveOrder.householdOrders = householdOrders.filter(ho => ho.orderId == collectiveOrder.id)
        }

        for(let h of households) {
          h.pastHouseholdOrders = pastHouseholdOrders.filter(pho => pho.householdId == h.id)
          h.currentHouseholdOrder = collectiveOrder && (householdOrders.filter(ho => ho.householdId == h.id && ho.orderId == collectiveOrder.id)[0] || null)
          h.householdPayments = householdPayments.filter(p => p.householdId == h.id)
        }

        for(let po of pastCollectiveOrders) {
          po.pastHouseholdOrders = pastHouseholdOrders.filter(ho => ho.orderId == po.id)
        }

        return {
          collectiveOrder,
          pastCollectiveOrders,
          householdOrders,
          pastHouseholdOrders,
          households,
          householdPayments,
          productCatalogue,
          categories,
          brands
        }                               
      })
  }
}

const command = {
  createOrder(householdId: number): Promise<number> {
    return Http.post(groupUrl(`/command/create-order/${householdId}`), {})
  },

  deleteOrder(orderId: number): Promise<number> {
    return Http.post(groupUrl(`/command/delete-order/${orderId}`), {})
  },

  placeOrder(orderId: number): Promise<number> {
    return Http.post(groupUrl(`/command/place-order/${orderId}`), {})
  },

  abandonOrder(orderId: number): Promise<number> {
    return Http.post(groupUrl(`/command/abandon-order/${orderId}`), {})
  },

  createHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/create-household-order/${orderId}/${householdId}`), {})
  },

  deleteHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(groupUrl(`/command/delete-household-order/${orderId}/${householdId}`), {})
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