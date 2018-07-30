export type Order = { id: number
                    , createdDate: Date
                    , total: number
                    , complete: boolean
                    }

export type Product = { id: number
                      , name: string
                      , price: number
                      }

export type Household = { id: number
                        , name: string
                        }