export type Order = { id: number
                    , createdDate: Date
                    , total: number
                    , complete: boolean
                    }

export type OrderSummary = { createdDate: Date
                           , complete: boolean
                           , households: OrderSummary_Household[]
                           , total: number
                           }

export type OrderSummary_Household = { id: number
                                     , name: string
                                     , total: number
                                     , cancelled: boolean
                                     }

export type HouseholdOrderSummary = { orderCreatedDate: Date
                                    , householdName: string 
                                    , cancelled: boolean
                                    , total: number
                                    , items: HouseholdOrderSummary_Item[]
                                    }

export type HouseholdOrderSummary_Item = { productId: number
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