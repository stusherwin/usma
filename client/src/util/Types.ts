export type VatRate = 'Zero' | 'Standard' | 'Reduced'

export interface OrderItem { 
  productId: number
  productCode: string
  productName: string
  oldProductPriceExcVat: number | null
  oldProductPriceIncVat: number | null
  productVatRate: VatRate
  itemQuantity: number
  oldItemTotalExcVat: number | null
  oldItemTotalIncVat: number | null
  productDiscontinued: boolean
  productPriceExcVat: number
  productPriceIncVat: number
  itemTotalExcVat: number
  itemTotalIncVat: number
  biodynamic: boolean
  fairTrade: boolean
  glutenFree: boolean
  organic: boolean
  addedSugar: boolean
  vegan: boolean
}

export interface Order { 
  totalExcVat: number
  totalIncVat: number
  items: OrderItem[]
  isComplete: boolean
  isAbandoned: boolean
}

export interface ProductCatalogueEntry { 
  code: string
  name: string
  priceExcVat: number
  priceIncVat: number
  vatRate: VatRate
  biodynamic: boolean
  fairTrade: boolean
  glutenFree: boolean
  organic: boolean
  addedSugar: boolean
  vegan: boolean
  category: string
  brand: string
}

export interface Household { 
  id: number
  name: string
  contactName: string | null
  contactEmail: string | null
  contactPhone: string | null
  totalOrders: number
  totalPayments: number
  balance: number
  currentHouseholdOrder: HouseholdOrder | null
  pastHouseholdOrders: PastHouseholdOrder[]
  householdPayments: HouseholdPayment[]
}
   
export interface CollectiveOrder extends Order { 
  id: number
  createdDate: Date
  createdBy: number
  createdByName: string
  isComplete: boolean
  oldTotalExcVat: number | null
  oldTotalIncVat: number | null
  totalExcVat: number
  totalIncVat: number
  allHouseholdsUpToDate: boolean
  items: OrderItem[]
  householdOrders: HouseholdOrder[]
}

export interface PastCollectiveOrder extends Order { 
  id: number
  createdDate: Date
  createdBy: number
  createdByName: string
  isAbandoned: boolean
  totalExcVat: number
  totalIncVat: number
  items: OrderItem[]
  pastHouseholdOrders: PastHouseholdOrder[]
}

export interface HouseholdOrder extends Order { 
  orderId: number
  orderCreatedDate: Date
  orderCreatedBy: number
  orderCreatedByName: string
  householdId: number
  householdName: string 
  isComplete: boolean
  isAbandoned: boolean
  isOpen: boolean
  oldTotalExcVat: number | null
  oldTotalIncVat: number | null
  totalExcVat: number
  totalIncVat: number
  items: OrderItem[]
}

export interface PastHouseholdOrder extends Order { 
  orderId: number
  orderCreatedDate: Date
  orderCreatedBy: number
  orderCreatedByName: string
  householdId: number
  householdName: string 
  isAbandoned: boolean
  totalExcVat: number
  totalIncVat: number
  items: OrderItem[]
}

export interface HouseholdPayment { 
  id: number 
  householdId: number
  date: Date
  amount: number
}