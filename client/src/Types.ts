export type VatRate = 'Zero' | 'Standard' | 'Reduced'

export type Product = { id: number
                      , code: string
                      , name: string
                      , price: number
                      , vatRate: VatRate
                      , priceUpdated: Date | null
                      }

export type ProductCatalogueEntry = { code: string
                                    , name: string
                                    , price: number
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
                              , isComplete: boolean
                              , isCancelled: boolean
                              , isPlaced: boolean
                              , isPast: boolean
                              , status: 'Open' | 'Complete' | 'Cancelled' | 'Placed'
                              , canBeAmended: boolean
                              , total: number
                              , items: OrderItem[]
                              }

export type HouseholdOrder = { orderId: number
                             , orderCreatedDate: Date
                             , isOrderPast: boolean
                             , householdId: number
                             , householdName: string 
                             , isComplete: boolean
                             , isCancelled: boolean
                             , isOpen: boolean
                             , status: 'Open' | 'Complete' | 'Cancelled' | 'Placed'
                             , canBeAmended: boolean
                             , total: number
                             , items: OrderItem[]
                             }

export type OrderItem = { productId: number
                        , productCode: string
                        , productName: string
                        , itemQuantity: number
                        , itemTotal: number
                        }

export type HouseholdPayment = { id: number 
                               , householdId: number
                               , date: Date
                               , amount: number
                               }