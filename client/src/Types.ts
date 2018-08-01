export type OrderList_Order = { id: number
                              , createdDate: Date
                              , total: number
                              , complete: boolean
                              }

export type Order_Details = { id: number
                            , createdDate: Date
                            , complete: boolean
                            , households: Order_Household[]
                            , total: number
                            }

export type Order_Household = { id: number 
                              , name: string
                              , total: number
                              , status: 'paid' | 'unpaid'
                              }

export type ProductList_Product = { id: number
                                  , name: string
                                  , price: number
                                  }

export type HouseholdList_Household = { id: number
                                      , name: string
                                      }