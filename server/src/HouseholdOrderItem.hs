{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate(..))

  data HouseholdOrderItem = HouseholdOrderItem { productId :: Int
                                               , productCode :: String
                                               , productName :: String
                                               , oldProductPriceExcVat :: Maybe Int
                                               , oldProductPriceIncVat :: Maybe Int
                                               , productVatRate :: VatRate
                                               , itemQuantity :: Int
                                               , oldItemTotalExcVat :: Maybe Int
                                               , oldItemTotalIncVat :: Maybe Int
                                               , productDiscontinued :: Bool
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
                                               } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItem