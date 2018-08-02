export type Order = { id: number
                    , createdDate: Date
                    , total: number
                    , complete: boolean
                    }

export type OrderSummary = { id: number
                           , createdDate: Date
                           , complete: boolean
                           , households: OrderSummary_Household[]
                           , total: number
                           }

export type OrderSummary_Household = { id: number 
                                     , name: string
                                     , total: number
                                     , status: 'paid' | 'unpaid' | 'cancelled'
                                     }

export type HouseholdOrder = { orderId: number
                             , orderCreatedDate: Date
                             , householdId: number
                             , householdName: string 
                             , paid: boolean
                             , cancelled: boolean
                             , items: HouseholdOrder_Item[]
                             , total: number
                             }

export type HouseholdOrder_Item = { productId: number
                                  , productName: string
                                  , quantity: number
                                  , total: number
                                  }

export type Product = { id: number
                      , name: string
                      , price: number
                      }

export type Household = { id: number
                        , name: string
                        }