{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module OrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate(..))

  data OrderItem = OrderItem { productId :: Int
                             , productCode :: String
                             , productName :: String
                             , productVatRate :: VatRate
                             , productPriceExcVat :: Int
                             , productPriceIncVat :: Int
                             , itemQuantity :: Int
                             , itemTotalExcVat :: Int
                             , itemTotalIncVat :: Int
                             , biodynamic :: Bool
                             , fairTrade :: Bool
                             , glutenFree :: Bool
                             , organic :: Bool
                             , addedSugar :: Bool
                             , vegan :: Bool
                             , adjustment :: Maybe OrderItemAdjustment
                             } deriving (Eq, Show, Generic)
  instance ToJSON OrderItem

  data OrderItemAdjustment = OrderItemAdjustment { oldProductPriceExcVat :: Int
                                                 , oldProductPriceIncVat :: Int
                                                 , oldItemQuantity :: Int
                                                 , oldItemTotalExcVat :: Int
                                                 , oldItemTotalIncVat :: Int
                                                 , productDiscontinued :: Bool
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON OrderItemAdjustment

  orderItem          productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan
    = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing
  
  householdOrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan (Just oldProductPriceExcVat) (Just oldProductPriceIncVat) (Just oldItemQuantity) (Just oldItemTotalExcVat) (Just oldItemTotalIncVat) (Just productDiscontinued)
    = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan $ Just $ OrderItemAdjustment oldProductPriceExcVat oldProductPriceIncVat oldItemQuantity oldItemTotalExcVat oldItemTotalIncVat productDiscontinued

  householdOrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan _ _ _ _ _ _
    = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing                     
  
  data OrderAdjustment = OrderAdjustment { oldTotalExcVat :: Int
                                         , oldTotalIncVat :: Int 
                                         } deriving (Eq, Show, Generic)
  instance ToJSON OrderAdjustment

  data HouseholdQuantityDetails = HouseholdQuantityDetails { hqdHouseholdId :: Int
                                                           , hqdItemQuantity :: Int
                                                           } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdQuantityDetails
  instance FromJSON HouseholdQuantityDetails

  data ReconcileOrderItemDetails = ReconcileOrderItemDetails { roidProductPriceExcVat :: Int
                                                             , roidHouseholdQuantities :: [HouseholdQuantityDetails]
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON ReconcileOrderItemDetails
  instance FromJSON ReconcileOrderItemDetails