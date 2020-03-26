export type VatRate = 'Zero' | 'Standard' | 'Reduced'

export interface OrderItem { 
  productId: number
  productCode: string
  productName: string
  productVatRate: VatRate
  itemQuantity: number
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
  adjustment: OrderItemAdjustment | null
  reconciled: boolean
}

export interface OrderItemAdjustment {
  oldProductPriceExcVat: number
  oldProductPriceIncVat: number
  oldItemQuantity: number
  oldItemTotalExcVat: number
  oldItemTotalIncVat: number
  productDiscontinued: boolean
}

export interface Order { 
  items: OrderItem[]
  totalExcVat: number
  totalIncVat: number
  isComplete: boolean
  isOpen: boolean
  isAbandoned: boolean
  isReconciled: boolean
  orderCreatedDate: Date
  orderCreatedBy: number
  orderCreatedByName: string
  orderIsPlaced: boolean
  orderIsAbandoned: boolean
  adjustment: OrderAdjustment | null
}

export interface OrderAdjustment {
  oldTotalExcVat: number
  oldTotalIncVat: number
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
  allHouseholdsUpToDate: boolean
  householdOrders: HouseholdOrder[]
}

export interface HouseholdOrder extends Order { 
  orderId: number
  householdId: number
  householdName: string 
}

export interface HouseholdPayment { 
  id: number 
  householdId: number
  date: Date
  amount: number
}

export interface GroupSettings {
  enablePayments: boolean
}