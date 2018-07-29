import { Order } from './Types'

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
  static getOrders(): Promise<Order[]> {
    return Promise.resolve([
      { id: 1, createdDate: new Date(2018, 0, 1), total: 31240, complete: false },
      { id: 2, createdDate: new Date(2018, 0, 2), total: 29523, complete: true },
      { id: 3, createdDate: new Date(2018, 0, 3), total: 45210, complete: true },
      { id: 4, createdDate: new Date(2018, 0, 4), total: 11200, complete: true }
    ])
    // const req = new Request(`/api/orders`)
    // return fetchHttpRequest(req, res => (res as ApiOrder[]).map(toOrder))
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
