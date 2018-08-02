import { Order, OrderSummary, HouseholdOrder, Product, Household } from './Types'
import { setTimeout } from 'timers';

type ApiOrder = { oId: number
                , oDate: ApiDate
                }

type ApiDate = { year: number
               , month: number
               , day: number
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

export class ServerApi {
  private static productList: Product[] = [
    { id: 1, name: 'Jam', price: 1240 },
    { id: 2, name: 'Butter', price: 9523 },
    { id: 3, name: 'Milk', price: 5210 },
    { id: 4, name: 'Bananas', price: 1200 }
  ]

  private static householdList: Household[] = [
    { id: 1, name: '123 Front Road' },
    { id: 2, name: '1 Main Terrace' },
    { id: 3, name: '24 The Street' },
    { id: 4, name: '3 Bowling Alley' }
  ]

  private static orderList: Order[] = [
    { id: 1, createdDate: new Date(2018, 0, 1), total: 31240, complete: true },
    { id: 2, createdDate: new Date(2018, 0, 2), total: 29523, complete: true },
    { id: 3, createdDate: new Date(2018, 0, 3), total: 45210, complete: true },
    { id: 4, createdDate: new Date(2018, 0, 4), total: 11200, complete: true }
  ]

  private static orderDetails: OrderSummary[] = [
    { id: 1, createdDate: new Date(2018, 0, 1), total: 31240, complete: true, households: [
      { id: 1, name: '123 Front Road', status: 'paid', total: 6954 },
      { id: 2, name: '1 Main Terrace', status: 'paid', total: 4455 },
      { id: 3, name: '24 The Street', status: 'unpaid', total: 4636 },
      { id: 4, name: '3 Bowling Alley', status: 'unpaid', total: 10331 }
    ] },
  ]

  private static orderHouseholdDetails: HouseholdOrder[] = [
    { orderId: 1, orderCreatedDate: new Date(2018, 0, 1), householdId: 1, householdName: '123 Front Road', paid: true, cancelled: false, total: 6954, items: [
      { productId: 1, productName: 'Jam', quantity: 1, total: 1240 },
      { productId: 2, productName: 'Butter', quantity: 2, total: 9523 },
    ] },
  ]
  
  private static respond<T>(data: T) {
    return new Promise<T>((resolve, reject) => {
      setTimeout(() => resolve(data), 1000)
    })
  }

  private static fail<T>(error: string) {
    return new Promise<T>((resolve, reject) => {
      setTimeout(() => reject(error), 1000)
    })
  }

  static getOrderList(): Promise<Order[]> {
    return ServerApi.respond(ServerApi.orderList.slice())
    // const req = new Request(`/api/orders`)
    // return fetchHttpRequest(req, res => (res as ApiOrder[]).map(toOrder))
  }

  static getOrderDetails(id: number): Promise<OrderSummary> {
    let order = ServerApi.orderDetails.find(o => o.id == id)
    if(!order) return ServerApi.fail('Order not found')
    console.log(order)
    return ServerApi.respond(order)
  }

  static getOrderHouseholdDetails(orderId: number, householdId: number): Promise<HouseholdOrder> {
    let order = ServerApi.orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return ServerApi.fail('Order not found')
    console.log(order)
    return ServerApi.respond(order)
  }

  static getProductList(): Promise<Product[]> {
    return ServerApi.respond(ServerApi.productList.slice())
  }

  static getHouseholdList(): Promise<Household[]> {
    return ServerApi.respond(ServerApi.householdList.slice())
  }

  static newOrder(): Promise<number> {
    let maxId = !ServerApi.orderList.length ? 0 : Math.max(...ServerApi.orderList.map(o => o.id))
    let newId = maxId + 1
    ServerApi.orderList.push({ id: newId, createdDate: new Date(2018, 0, 5), total: 0, complete: false })
    return ServerApi.respond(newId)
  }

  static deleteOrder(id: number): Promise<{}> {
    ServerApi.orderList.splice(ServerApi.orderList.findIndex(o => o.id == id), 1)
    return ServerApi.respond({})
  }

  static addHouseholdOrderItem(orderId: number, householdId: number, productId: number, quantity: number): Promise<{}> {
    let order = ServerApi.orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return ServerApi.fail('Order not found')
    let product = ServerApi.productList.find(p => p.id == productId)
    if(!product) return ServerApi.fail('Product not found')    
    order.items.unshift({productId, productName: product.name, quantity: quantity, total: product.price * quantity})
    return ServerApi.respond({})    
  }

  static removeHouseholdOrderItem(orderId: number, householdId: number, productId: number): Promise<{}> {
    let order = ServerApi.orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return ServerApi.fail('Order not found')
    let itemIndex = order.items.findIndex(i => i.productId == productId)
    order.items.splice(itemIndex, 1)
    return ServerApi.respond({})    
  }

  static updateHouseholdOrderItem(orderId: number, householdId: number, productId: number, quantity: number): Promise<{}> {
    let order = ServerApi.orderHouseholdDetails.find(o => o.orderId == orderId && o.householdId == householdId)
    if(!order) return ServerApi.fail('Order not found')
    let item = order.items.find(i => i.productId == productId)
    if(!item) return ServerApi.fail('Item not found')
    let product = ServerApi.productList.find(p => p.id == productId)
    if(!product) return ServerApi.fail('Product not found')    
    item.quantity = quantity
    item.total = quantity * product.price
    return ServerApi.respond({})    
  }
}

function fetchHttpRequest<T>(req: Request, process: (res: any) => T): Promise<T> {
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
      .then(res => Promise.resolve(process(res)))
      .catch(err => Promise.reject(new ApiError('Error from the server', 'Received an unexpected response from the server: ' + err.error, err.status || null)))
  } catch(TypeError) {
    return Promise.reject(new ApiError('Can\'t connect to the server', 'The server seems to be down or busy, please wait a while and try again.', null))
  }
}

function toDate(date: ApiDate): Date {
  return new Date(Date.UTC(date.year, date.month - 1, date.day))
}

function toOrder(o: ApiOrder): Order {
  return {
    id: o.oId,
    createdDate: toDate(o.oDate),
    total: 31240,
    complete: true,
  }
}
