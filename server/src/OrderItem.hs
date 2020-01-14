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
                             , itemQuantity :: Int
                             , productPriceExcVat :: Int
                             , productPriceIncVat :: Int
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
                                                 , oldItemTotalExcVat :: Int
                                                 , oldItemTotalIncVat :: Int
                                                 , productDiscontinued :: Bool
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON OrderItemAdjustment

  orderItem productId productCode productName productPriceExcVat productPriceIncVat productVatRate itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan
    = OrderItem productId productCode productName productVatRate itemQuantity productPriceExcVat productPriceIncVat itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing
  
  householdOrderItem productId productCode productName _ _ productVatRate itemQuantity _ _ False productPriceExcVat productPriceIncVat itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan False
    = OrderItem productId productCode productName productVatRate itemQuantity productPriceExcVat productPriceIncVat itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing
                     
  householdOrderItem productId productCode productName oldProductPriceExcVat oldProductPriceIncVat productVatRate itemQuantity oldItemTotalExcVat oldItemTotalIncVat productDiscontinued productPriceExcVat productPriceIncVat itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan True
    = OrderItem productId productCode productName productVatRate itemQuantity productPriceExcVat productPriceIncVat itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan $ Just $ OrderItemAdjustment oldProductPriceExcVat oldProductPriceIncVat oldItemTotalExcVat oldItemTotalIncVat productDiscontinued

  data OrderAdjustment = OrderAdjustment { oldTotalExcVat :: Int
                                         , oldTotalIncVat :: Int 
                                         } deriving (Eq, Show, Generic)
  instance ToJSON OrderAdjustment