import { Order, Product, Household } from './Types'
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
  private static orders = [
    { id: 1, createdDate: new Date(2018, 0, 1), total: 31240, complete: true },
    { id: 2, createdDate: new Date(2018, 0, 2), total: 29523, complete: true },
    { id: 3, createdDate: new Date(2018, 0, 3), total: 45210, complete: true },
    { id: 4, createdDate: new Date(2018, 0, 4), total: 11200, complete: true }
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

  static getOrders(): Promise<Order[]> {
    return ServerApi.respond(ServerApi.orders.slice())
    // const req = new Request(`/api/orders`)
    // return fetchHttpRequest(req, res => (res as ApiOrder[]).map(toOrder))
  }

  static getOrder(id: number): Promise<Order> {
    let order = ServerApi.orders.find(o => o.id == id)
    if(!order) return ServerApi.fail('Order not found')
    console.log(order)
    return ServerApi.respond(order)
  }

  static newOrder(): Promise<number> {
    let maxId = !ServerApi.orders.length ? 0 : Math.max(...ServerApi.orders.map(o => o.id))
    let newId = maxId + 1
    ServerApi.orders.push({ id: newId, createdDate: new Date(2018, 0, 5), total: 33230, complete: false })
    return ServerApi.respond(newId)
  }

  static deleteOrder(id: number): Promise<{}> {
    ServerApi.orders.splice(ServerApi.orders.findIndex(o => o.id == id), 1)
    return ServerApi.respond({})
  }

  static getProducts(): Promise<Product[]> {
    return ServerApi.respond([
      { id: 1, name: 'Jam', price: 1240 },
      { id: 2, name: 'Butter', price: 9523 },
      { id: 3, name: 'Milk', price: 5210 },
      { id: 4, name: 'Bananas', price: 1200 }
    ])
  }

  static getHouseholds(): Promise<Household[]> {
    return ServerApi.respond([
      { id: 1, name: '123 Front Road' },
      { id: 2, name: '1 Main Terrace' },
      { id: 3, name: '24 The Street' },
      { id: 4, name: '3 Bowling Alley' }
    ])
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
    complete: true
  }
}
