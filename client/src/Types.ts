export type Product = { id: number
                      , name: string
                      , price: number
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
                              , isPaid: boolean
                              , status: 'Open' | 'Complete' | 'Cancelled' | 'Paid' | 'Placed'
                              , canBeAmended: boolean
                              , total: number
                              , items: OrderItem[]
                              }

export type HouseholdOrder = { orderId: number
                             , orderCreatedDate: Date
                             , isOrderPast: boolean
                             , householdId: number
                             , householdName: string 
                             , householdBalance: number 
                             , isComplete: boolean
                             , isCancelled: boolean
                             , isOpen: boolean
                             , isPaid: boolean
                             , canBeAmended: boolean
                             , total: number
                             , items: OrderItem[]
                             }

export type OrderItem = { productId: number
                        , productName: string
                        , itemQuantity: number
                        , itemTotal: number
                        }

export type HouseholdPayment = { id: number 
                               , householdId: number
                               , date: Date
                               , amount: number
                               }