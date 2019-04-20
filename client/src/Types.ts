export type VatRate = 'Zero' | 'Standard' | 'Reduced'

export type Product = { id: number
                      , code: string
                      , name: string
                      , priceExcVat: number
                      , priceIncVat: number
                      , vatRate: VatRate
                      , priceUpdated: Date | null
                      }

export type ProductCatalogueEntry = { code: string
                                    , name: string
                                    , priceExcVat: number
                                    , priceIncVat: number
                                    , vatRate: VatRate
                                    }

export type Household = { id: number
                        , name: string
                        , totalOrders: number
                        , totalPayments: number
                        , balance: number
                        }

export type CollectiveOrder = { id: number
                              , createdDate: Date
                              , createdBy: number
                              , createdByName: string
                              , isComplete: boolean
                              , status: 'Open' | 'Complete'
                              , totalExcVat: number
                              , totalIncVat: number
                              , items: OrderItem[]
                              }

export type PastCollectiveOrder = { id: number
                                  , createdDate: Date
                                  , createdBy: number
                                  , createdByName: string
                                  , isAbandoned: boolean
                                  , totalExcVat: number
                                  , totalIncVat: number
                                  , items: OrderItem[]
                                  }

export type HouseholdOrder = { orderId: number
                             , orderCreatedDate: Date
                             , orderCreatedBy: number
                             , orderCreatedByName: string
                             , householdId: number
                             , householdName: string 
                             , isComplete: boolean
                             , isAbandoned: boolean
                             , isOpen: boolean
                             , status: 'Open' | 'Complete' | 'Abandoned'
                             , totalExcVat: number
                             , totalIncVat: number
                             , items: OrderItem[]
                             }

export type PastHouseholdOrder = { orderId: number
                                 , orderCreatedDate: Date
                                 , orderCreatedBy: number
                                 , orderCreatedByName: string
                                 , householdId: number
                                 , householdName: string 
                                 , isAbandoned: boolean
                                 , totalExcVat: number
                                 , totalIncVat: number
                                 , items: OrderItem[]
                                 }

export type OrderItem = { productId: number
                        , productCode: string
                        , productName: string
                        , productPriceExcVat: number
                        , productPriceIncVat: number
                        , productVatRate: VatRate
                        , itemQuantity: number
                        , itemTotalExcVat: number
                        , itemTotalIncVat: number
                        }

export type HouseholdPayment = { id: number 
                               , householdId: number
                               , date: Date
                               , amount: number
                               }