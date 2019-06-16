export type VatRate = 'Zero' | 'Standard' | 'Reduced'

export type ProductCatalogueEntry = { code: string
                                    , name: string
                                    , priceExcVat: number
                                    , priceIncVat: number
                                    , vatRate: VatRate
                                    , biodynamic: boolean
                                    , fairTrade: boolean
                                    , glutenFree: boolean
                                    , organic: boolean
                                    , addedSugar: boolean
                                    , vegan: boolean
                                    }

export type Household = { id: number
                        , name: string
                        , contactName: string | null
                        , contactEmail: string | null
                        , contactPhone: string | null
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
                              , oldTotalExcVat: number | null
                              , oldTotalIncVat: number | null
                              , totalExcVat: number
                              , totalIncVat: number
                              , allHouseholdsUpToDate: boolean
                              , items: OrderItem[]
                              }

export type PastCollectiveOrder = { id: number
                                  , createdDate: Date
                                  , createdBy: number
                                  , createdByName: string
                                  , isAbandoned: boolean
                                  , totalExcVat: number
                                  , totalIncVat: number
                                  , items: PastOrderItem[]
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
                             , oldTotalExcVat: number | null
                             , oldTotalIncVat: number | null
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
                                 , items: PastOrderItem[]
                                 }

export type OrderItem = { productId: number
                        , productCode: string
                        , productName: string
                        , oldProductPriceExcVat: number | null
                        , oldProductPriceIncVat: number | null
                        , productVatRate: VatRate
                        , itemQuantity: number
                        , oldItemTotalExcVat: number | null
                        , oldItemTotalIncVat: number | null
                        , productDiscontinued: boolean
                        , productPriceExcVat: number
                        , productPriceIncVat: number
                        , itemTotalExcVat: number
                        , itemTotalIncVat: number
                        }

export type HouseholdOrderItem = { productId: number
                                 , productCode: string
                                 , productName: string
                                 , oldProductPriceExcVat: number | null
                                 , oldProductPriceIncVat: number | null
                                 , productVatRate: VatRate
                                 , itemQuantity: number
                                 , oldItemTotalExcVat: number | null
                                 , oldItemTotalIncVat: number | null
                                 , productDiscontinued: boolean
                                 , productPriceExcVat: number
                                 , productPriceIncVat: number
                                 , itemTotalExcVat: number
                                 , itemTotalIncVat: number
                                 }

export type PastOrderItem = { productId: number
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