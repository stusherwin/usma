import { Order, OrderSummary, HouseholdOrderSummary, Product, Household, OrderSummary_Household, HouseholdOrderSummary_Item } from './Types'
import { Util } from './Util'
import { setTimeout } from 'timers';

let query = {
  orders(): Promise<Order[]> {
    return Http.get<ApiOrder[]>('/api/query/orders')
               .then(res => res.map(toOrder))
  },

  orderSummary(orderId: number): Promise<OrderSummary> {
    return Http.get<ApiOrderSummary>(`/api/query/order-summary/${orderId}`)
               .then(toOrderSummary)
  },

  householdOrderSummary(orderId: number, householdId: number): Promise<HouseholdOrderSummary> {
    return Http.get<ApiHouseholdOrderSummary>(`/api/query/household-order-summary/${orderId}/${householdId}`)
               .then(toHouseholdOrderSummary)
  },

  products(): Promise<Product[]> {
    return Http.get<ApiProduct[]>('/api/query/products')
               .then(res => res.map(toProduct))
  },

  households(): Promise<Household[]> {
    return Http.get<ApiHousehold[]>('/api/query/households')
               .then(res => res.map(toHousehold))
  },
}

let command = {
  newOrder(date: Date): Promise<number> {
    return Http.post(`/api/command/create-order/${Util.dateString(date)}`, {})
  },

  deleteOrder(id: number): Promise<{}> {
    return Http.post(`/api/command/delete-order/${id}`, {})
  },

  ensureHouseholdOrderItem(orderId: number, householdId: number, productId: number, quantity: number): Promise<{}> {
    return Http.post(`/api/command/ensure-household-order-item`, { ehoiOrderId: orderId
                                                                 , ehoiHouseholdId: householdId
                                                                 , ehoiProductId: productId
                                                                 , ehoiQuantity: quantity
                                                                 })
  },

  removeHouseholdOrderItem(orderId: number, householdId: number, productId: number): Promise<{}> {
    return Http.post(`/api/command/remove-household-order-item`, { rhoiOrderId: orderId
                                                                 , rhoiHouseholdId: householdId
                                                                 , rhoiProductId: productId
                                                                 })
  },

  cancelHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(`/api/command/cancel-household-order`, { choOrderId: orderId
                                                            , choHouseholdId: householdId
                                                            })
  },

  uncancelHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(`/api/command/uncancel-household-order`, { choOrderId: orderId
                                                              , choHouseholdId: householdId
                                                              })
  },
}

export let ServerApi = {
  query,
  command
}

export class Http {
  static get<T>(url: string /*, query?: {[key: string]: string}*/): Promise<T> {
    return this.fetchHttpRequest(new Request(url /*+ this.queryString(query)*/))
  }
  
  static post<T>(url: string/*, query: {[key: string]: string}*/, body: {}): Promise<T> {
    return this.fetchHttpRequest(new Request(url /*+ this.queryString(query)*/,
      { method: 'POST'
      , headers: new Headers({'Content-Type' : 'application/json'})
      , body: JSON.stringify(body)
      }))
  }

  private static queryString(query?: {[key: string]: string}): string {
    if(!query) return ''

    let qs = []
    for(let key in query) {
      qs.push(key + '=' + encodeURIComponent(query[key]))
    }
    if(!qs.length) return ''

    return '?' + qs.join('&')
  }

  private static fetchHttpRequest<T>(req: Request): Promise<T> {
    try {
      return fetch(req, {credentials: 'same-origin'})
        .then(res => {
          if(!res.ok) {
            return res.text().then(txt => { throw new ApiError(`${res.statusText} (${res.status})`, txt, res.status) })
          }
          let contentType = res.headers.get('content-type')
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

type ApiOrder = { oId: number
                , oCreatedDate: string
                , oComplete: boolean
                , oTotal: number
                }

type ApiProduct = { pId: number
                  , pName: string
                  , pPrice: number
                  }

type ApiHousehold = { hId: number
                    , hName: string
                    }

type ApiOrderSummary = { osCreatedDate: string
                       , osComplete: boolean
                       , osTotal: number
                       , osHouseholds: ApiOrderSummary_Household[]
                       }

type ApiOrderSummary_Household = { oshId: number
                                 , oshName: string
                                 , oshCancelled: boolean
                                 , oshTotal: number
                                 }

type ApiHouseholdOrderSummary = { hosOrderCreatedDate: string
                                , hosOrderComplete: boolean
                                , hosHouseholdName: string 
                                , hosCancelled: boolean
                                , hosTotal: number
                                , hosItems: ApiHouseholdOrderSummary_Item[]
                                }

type ApiHouseholdOrderSummary_Item = { hosiProductId: number
                                     , hosiProductName: string
                                     , hosiQuantity: number
                                     , hosiTotal: number
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

function toOrder(o: ApiOrder): Order {
  return {
    id: o.oId,
    createdDate: new Date(o.oCreatedDate),
    complete: o.oComplete,
    total: o.oTotal,
  }
}

function toProduct(p: ApiProduct): Product {
  return {
    id: p.pId,
    name: p.pName,
    price: p.pPrice
  }
}

function toHousehold(h: ApiHousehold): Household {
  return {
    id: h.hId,
    name: h.hName,
  }
}

function toOrderSummary(o: ApiOrderSummary): OrderSummary {
  return {
    createdDate: new Date(o.osCreatedDate),
    complete: o.osComplete,
    total: o.osTotal,
    households: o.osHouseholds.map(toOrderSummary_Household)
  }
}

function toOrderSummary_Household(o: ApiOrderSummary_Household): OrderSummary_Household {
  return {
    id: o.oshId, 
    name: o.oshName,
    total: o.oshTotal,
    cancelled: o.oshCancelled
  }
}

function toHouseholdOrderSummary(o: ApiHouseholdOrderSummary): HouseholdOrderSummary {
  return {
    orderCreatedDate: new Date(o.hosOrderCreatedDate),
    orderComplete: o.hosOrderComplete,
    householdName: o.hosHouseholdName, 
    cancelled: o.hosCancelled,
    total: o.hosTotal,
    items: o.hosItems.map(toHouseholdOrderSummary_Item)
  }
}

function toHouseholdOrderSummary_Item(o: ApiHouseholdOrderSummary_Item): HouseholdOrderSummary_Item {
  return {
    productId: o.hosiProductId, 
    productName: o.hosiProductName,
    quantity: o.hosiQuantity,
    total: o.hosiTotal
  }
}

function respond<T>(data: T) {
  return new Promise<T>((resolve, reject) => {
    setTimeout(() => resolve(data), 1000)
  })
}

function fail<T>(error: string) {
  return new Promise<T>((resolve, reject) => {
    setTimeout(() => reject(error), 1000)
  })
}