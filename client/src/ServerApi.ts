import { Order, OrderSummary, HouseholdOrderSummary, Product, Household } from './Types'
import { Util } from './Util'
import { setTimeout } from 'timers';

type ApiOrder = { oId: string
                , oCreatedDate: string
                }

type ApiOrderSummary = { osId: string
                       , osCreatedDate: string
                       , osComplete: boolean
                       , osHouseholds: ApiOrderSummary_Household[]
                       , osTotal: number
                       }

type ApiOrderSummary_Household = { oshId: number 
                                 , oshName: string
                                 , oshTotal: number
                                 , oshStatus: 'paid' | 'unpaid' | 'cancelled'
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
  { id: 1, name: 'Jam', price: 1240 },
  { id: 2, name: 'Butter', price: 9523 },
  { id: 3, name: 'Milk', price: 5210 },
  { id: 4, name: 'Bananas', price: 1200 }
]

let householdList: Household[] = [
  { id: 1, name: '123 Front Road' },
  { id: 2, name: '1 Main Terrace' },
  { id: 3, name: '24 The Street' },
  { id: 4, name: '3 Bowling Alley' }
]

let orderList: Order[] = [
  { id: '2018-01-01', createdDate: new Date(2018, 0, 1), total: 31240, complete: true },
  { id: '2018-01-02', createdDate: new Date(2018, 0, 2), total: 29523, complete: true },
  { id: '2018-01-03', createdDate: new Date(2018, 0, 3), total: 45210, complete: true },
  { id: '2018-01-04', createdDate: new Date(2018, 0, 4), total: 11200, complete: true }
]

let orderDetails: OrderSummary[] = [
  { id: '2018-01-01', createdDate: new Date(2018, 0, 1), total: 31240, complete: true, households: [
    { id: 1, name: '123 Front Road', status: 'paid', total: 6954 },
    { id: 2, name: '1 Main Terrace', status: 'paid', total: 4455 },
    { id: 3, name: '24 The Street', status: 'unpaid', total: 4636 },
    { id: 4, name: '3 Bowling Alley', status: 'unpaid', total: 10331 }
  ] },
]

let orderHouseholdDetails: HouseholdOrderSummary[] = [
  { orderId: '2018-01-01', orderCreatedDate: new Date(2018, 0, 1), householdId: 1, householdName: '123 Front Road', paid: true, cancelled: false, total: 6954, items: [
    { productId: 1, productName: 'Jam', quantity: 1, total: 1240 },
    { productId: 2, productName: 'Butter', quantity: 2, total: 9523 },
  ] },
]

let query = {
  orders(): Promise<Order[]> {
    // return respond(orderList.slice())
    const req = new Request(`/api/query/orders`)
    return fetchHttpRequest(req, res => (res as ApiOrder[]).map(toOrder))
  },

  orderSummary(id: string): Promise<OrderSummary> {
    // let order = orderDetails.find(o => o.id == id)
    // if(!order) return fail('Order not found')
    // console.log(order)
    // return respond(order)
    const req = new Request(`/api/query/order-summary/${id}`)
    return fetchHttpRequest(req, res => toOrderSummary(res as ApiOrderSummary))
  },

  householdOrderSummary(orderId: string, householdId: number): Promise<HouseholdOrderSummary> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    console.log(order)
    return respond(order)
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
    // let maxId = !orderList.length ? 0 : Math.max(...orderList.map(o => o.id))
    // let newId = maxId + 1
    // orderList.push({ id: newId, createdDate: new Date(2018, 0, 5), total: 0, complete: false })
    // return respond(newId)
    const req = new Request(`/api/command/create-order/${Util.dateString(date)}`,
                              { method: 'POST'
                              , headers: new Headers({'Content-Type' : 'application/json'})
                              })
    return fetchHttpRequest(req)
  },

  deleteOrder(id: string): Promise<{}> {
    orderList.splice(orderList.findIndex(o => o.id == id), 1)
    return respond({})
  },

  addHouseholdOrderItem(orderId: string, householdId: number, productId: number, quantity: number): Promise<{}> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    let product = productList.find(p => p.id == productId)
    if(!product) return fail('Product not found')    
    order.items.unshift({productId, productName: product.name, quantity: quantity, total: product.price * quantity})
    return respond({})    
  },

  removeHouseholdOrderItem(orderId: string, householdId: number, productId: number): Promise<{}> {
    let order = orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return fail('Order not found')
    let itemIndex = order.items.findIndex(i => i.productId == productId)
    order.items.splice(itemIndex, 1)
    return respond({})    
  },

  updateHouseholdOrderItem(orderId: string, householdId: number, productId: number, quantity: number): Promise<{}> {
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

function fetchHttpRequest<T>(req: Request, process?: (res: any) => T): Promise<T> {
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
      .then(res => Promise.resolve(process? process(res) : res))
      .catch(err => Promise.reject(new ApiError('Error from the server', 'Received an unexpected response from the server: ' + err.error, err.status || null)))
  } catch(TypeError) {
    return Promise.reject(new ApiError('Can\'t connect to the server', 'The server seems to be down or busy, please wait a while and try again.', null))
  }
}

function toOrder(o: ApiOrder): Order {
  return {
    id: o.oId,
    createdDate: new Date(o.oCreatedDate),
    total: 31240,
    complete: true,
  }
}

function toOrderSummary(o: ApiOrderSummary): OrderSummary {
  return {
    id: o.osId,
    createdDate: new Date(o.osCreatedDate),
    total: o.osTotal,
    complete: o.osComplete,
    households: []
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