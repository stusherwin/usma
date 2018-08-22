import { Product, Household, CollectiveOrder, HouseholdOrder } from './Types'
import { Util } from './Util'
import { setTimeout } from 'timers';

const query = {
  collectiveOrders(): Promise<CollectiveOrder[]> {
    return Http.get<CollectiveOrder[]>('/api/query/collective-orders')
      .then(res => { res.forEach(o => o.createdDate = new Date(o.createdDate)); return res })
  },

  householdOrders(): Promise<HouseholdOrder[]> {
    return Http.get<HouseholdOrder[]>('/api/query/household-orders')
      .then(res => { res.forEach(o => o.orderCreatedDate = new Date(o.orderCreatedDate)); return res })
  },

  products(): Promise<Product[]> {
    return Http.get<Product[]>('/api/query/products')
  },

  households(): Promise<Household[]> {
    return Http.get<Household[]>('/api/query/households')
  },
}

const command = {
  createOrder(householdId?: number): Promise<number> {
    return Http.post(`/api/command/create-order/${householdId}`, {})
  },

  addHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(`/api/command/add-household-order`, { choOrderId: orderId
                                                         , choHouseholdId: householdId
                                                         })
  },

  removeHouseholdOrder(orderId: number, householdId: number): Promise<{}> {
    return Http.post(`/api/command/remove-household-order`, { choOrderId: orderId
                                                            , choHouseholdId: householdId
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

  createHousehold(name: string): Promise<number> {
    return Http.post(`/api/command/create-household/`, { chName: name })
  },

  createProduct(name: string, price: number): Promise<number> {
    return Http.post(`/api/command/create-product/`, { cpName: name
                                                     , cpPrice: price
                                                     })
  },

  archiveProduct(id: number): Promise<{}> {
    return Http.post(`/api/command/archive-product/${id}`, {})
  },

  archiveHousehold(id: number): Promise<{}> {
    return Http.post(`/api/command/archive-household/${id}`, {})
  },
}

export const ServerApi = {
  query,
  command
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