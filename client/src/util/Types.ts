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
  items: OrderItem[]
  totalExcVat: number
  totalIncVat: number
  oldTotalExcVat: number | null
  oldTotalIncVat: number | null
  isComplete: boolean
  isAbandoned: boolean
  isOpen: boolean
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
  currentHouseholdOrder: HouseholdOrder | undefined
  pastHouseholdOrders: HouseholdOrder[]
  householdPayments: HouseholdPayment[]
}
   
export interface CollectiveOrder extends Order { 
  id: number
  createdDate: Date
  createdBy: number
  createdByName: string
  allHouseholdsUpToDate: boolean
  householdOrders: HouseholdOrder[]
}

export interface HouseholdOrder extends Order { 
  orderId: number
  orderCreatedDate: Date
  orderCreatedBy: number
  orderCreatedByName: string
  householdId: number
  householdName: string 
}

export interface HouseholdPayment { 
  id: number 
  householdId: number
  date: Date
  amount: number
}