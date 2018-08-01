export type OrderList_Order = { id: number
                              , createdDate: Date
                              , total: number
                              , complete: boolean
                              }

export type Order = { id: number
                    , createdDate: Date
                    , complete: boolean
                    , households: Order_Household[]
                    , total: number
                    }

export type Order_Household = { id: number 
                              , name: string
                              , total: number
                              , status: 'paid' | 'unpaid' | 'cancelled'
                              }

export type OrderHousehold = { orderId: number
                             , orderCreatedDate: Date
                             , householdId: number
                             , householdName: string 
                             , paid: boolean
                             , cancelled: boolean
                             , items: OrderHousehold_Item[]
                             , total: number
                             }

export type OrderHousehold_Item = { productId: number
                                  , productName: string
                                  , quantity: number
                                  , total: number
                                  }

export type ProductList_Product = { id: number
                                  , name: string
                                  , price: number
                                  }

export type HouseholdList_Household = { id: number
                                      , name: string
                                      }