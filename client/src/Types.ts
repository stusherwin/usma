export type Order = { id: string
                    , createdDate: Date
                    , total: number
                    , complete: boolean
                    }

export type OrderSummary = { id: string
                           , createdDate: Date
                           , complete: boolean
                           , households: OrderSummary_Household[]
                           , total: number
                           }

export type OrderSummary_Household = { id: string 
                                     , name: string
                                     , total: number
                                     , status: 'paid' | 'unpaid' | 'cancelled'
                                     }

export type HouseholdOrderSummary = { orderId: string
                                    , orderCreatedDate: Date
                                    , householdId: string
                                    , householdName: string 
                                    , paid: boolean
                                    , cancelled: boolean
                                    , items: HouseholdOrderSummary_Item[]
                                    , total: number
                                    }

export type HouseholdOrderSummary_Item = { productId: string
                                         , productName: string
                                         , quantity: number
                                         , total: number
                                         }

export type Product = { id: string
                      , name: string
                      , price: number
                      }

export type Household = { id: string
                        , name: string
                        }