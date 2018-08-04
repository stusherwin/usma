import { Order, OrderSummary, HouseholdOrderSummary, Product, Household, OrderSummary_Household } from './Types'
import { Util } from './Util'
import { setTimeout } from 'timers';

type ApiOrder = { oId: string
                , oCreatedDate: string
                , oComplete: boolean
                , oTotal: number
                }

type ApiOrderSummary = { osId: string
                       , osCreatedDate: string
                       , osComplete: boolean
                       , osTotal: number
                       , osHouseholds: ApiOrderSummary_Household[]
                       }

type ApiOrderSummary_Household = { oshId: string 
                                 , oshName: string
                                 , oshTotal: number
                                 , oshStatus: 'paid' | 'unpaid' | 'cancelled'
                                 }

type ApiHouseholdOrderSummary = { hosOrderId: string
                                , hosOrderCreatedDate: Date
                                , hosHouseholdId: string
                                , hosHouseholdName: string 
                                , hosPaid: boolean
                                , hosCancelled: boolean
                                , hosItems: ApiHouseholdOrderSummary_Item[]
                                , hosTotal: number
                                }

type ApiHouseholdOrderSummary_Item = { hosiProductId: string
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

let productList: Product[] = [
  { id: '1', name: 'Jam', price: 1240 },
  { id: '2', name: 'Butter', price: 9523 },
  { id: '3', name: 'Milk', price: 5210 },
  { id: '4', name: 'Bananas', price: 1200 }
]

let householdList: Household[] = [
  { id: '1', name: '123 Front Road' },
  { id: '2', name: '1 Main Terrace' },
  { id: '3', name: '24 The Street' },
  { id: '4', name: '3 Bowling Alley' }
]

let orderList: Order[] = [
  { id: '2018-01-01', createdDate: new Date(2018, 0, 1), total: 31240, complete: true },
  { id: '2018-01-02', createdDate: new Date(2018, 0, 2), total: 29523, complete: true },
  { id: '2018-01-03', createdDate: new Date(2018, 0, 3), total: 45210, complete: true },
  { id: '2018-01-04', createdDate: new Date(2018, 0, 4), total: 11200, complete: true }
]

let orderDetails: OrderSummary[] = [
  { id: '2018-01-01', createdDate: new Date(2018, 0, 1), total: 31240, complete: true, households: [
    { id: '1', name: '123 Front Road', status: 'paid', total: 6954 },
    { id: '2', name: '1 Main Terrace', status: 'paid', total: 4455 },
    { id: '3', name: '24 The Street', status: 'unpaid', total: 4636 },
    { id: '4', name: '3 Bowling Alley', status: 'unpaid', total: 10331 }
  ] },
]

let orderHouseholdDetails: HouseholdOrderSummary[] = [
  { orderId: '2018-01-01', orderCreatedDate: new Date(2018, 0, 1), householdId: '1', householdName: '123 Front Road', paid: true, cancelled: false, total: 6954, items: [
    { productId: '1', productName: 'Jam', quantity: 1, total: 1240 },
    { productId: '2', productName: 'Butter', quantity: 2, total: 9523 },
  ] },
]

let query = {
  orders(): Promise<Order[]> {
    return Http.get<ApiOrder[]>('/api/query/orders')
               .then(res => res.map(toOrder))
  },

  orderSummary(orderId: string): Promise<OrderSummary> {
    return Http.get<ApiOrderSummary>(`/api/query/order-summary/${orderId}`)
               .then(toOrderSummary)
  },

  householdOrderSummary(orderId: string, householdId: string): Promise<HouseholdOrderSummary> {
    return Http.get<ApiHouseholdOrderSummary>(`/api/query/household-order-summary/${orderId}/${householdId}`)
               .then(toHouseholdOrderSummary)
  },

  products(): Promise<Product[]> {
    return respond(productList.slice())
  },

  households(): Promise<Household[]> {
    return respond(householdList.slice())
  },
}

let command = {
  newOrder(date: Date): Promise<{}> {
    return Http.post(`/api/command/create-order/${Util.dateString(date)}`, {})
  },

  deleteOrder(id: string): Promise<{}> {
    orderList.splice(orderList.findIndex(o => o.id == id), 1)
    return respond({})
  },

  addHouseholdOrderItem(orderId: string, householdId: string, productId: string, quantity: number): Promise<{}> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    let product = productList.find(p => p.id == productId)
    if(!product) return fail('Product not found')    
    order.items.unshift({productId, productName: product.name, quantity: quantity, total: product.price * quantity})
    return respond({})    
  },

  removeHouseholdOrderItem(orderId: string, householdId: string, productId: string): Promise<{}> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    let itemIndex = order.items.findIndex(i => i.productId == productId)
    order.items.splice(itemIndex, 1)
    return respond({})    
  },

  updateHouseholdOrderItem(orderId: string, householdId: string, productId: string, quantity: number): Promise<{}> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    let item = order.items.find(i => i.productId == productId)
    if(!item) return fail('Item not found')
    let product = productList.find(p => p.id == productId)
    if(!product) return fail('Product not found')    
    item.quantity = quantity
    item.total = quantity * product.price
    return respond({})    
  }
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

function toOrder(o: ApiOrder): Order {
  return {
    id: o.oId,
    createdDate: new Date(o.oCreatedDate),
    complete: o.oComplete,
    total: o.oTotal,
  }
}

function toOrderSummary(o: ApiOrderSummary): OrderSummary {
  return {
    id: o.osId,
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
    status: o.oshStatus
  }
}

function toHouseholdOrderSummary(o: ApiHouseholdOrderSummary): HouseholdOrderSummary {
  return {
    orderId: o.hosOrderId,
    orderCreatedDate: new Date(o.hosOrderCreatedDate),
    householdId: o.hosHouseholdId,
    householdName: o.hosHouseholdName, 
    paid: o.hosPaid,
    cancelled: o.hosCancelled,
    total: o.hosTotal,
    items: []
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