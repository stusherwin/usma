export type Product = { id: number
                      , name: string
                      , price: number
                      }

export type Household = { id: number
                        , name: string
                        }

export type CollectiveOrder = { id: number
                              , createdDate: Date
                              , complete: boolean
                              , cancelled: boolean
                              , total: number
                              , householdIds: number[]
                              , householdOrders: HouseholdOrder[]
                              , items: OrderItem[]
                              }

export type HouseholdOrder = { orderId: number
                             , orderCreatedDate: Date
                             , orderComplete: boolean
                             , householdId: number
                             , householdName: string 
                             , cancelled: boolean
                             , total: number
                             , items: OrderItem[]
                             }

export type OrderItem = { productId: number
                        , productName: string
                        , itemQuantity: number
                        , itemTotal: number
                        }