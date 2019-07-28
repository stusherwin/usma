{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastOrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate(..))
  
  data PastOrderItem = PastOrderItem { productId :: Int
                                     , productCode :: String
                                     , productName :: String
                                     , productPriceExcVat :: Int
                                     , productPriceIncVat :: Int
                                     , productVatRate :: VatRate
                                     , itemQuantity :: Int
                                     , itemTotalExcVat :: Int
                                     , itemTotalIncVat :: Int
                                     , biodynamic :: Bool
                                     , fairTrade :: Bool
                                     , glutenFree :: Bool
                                     , organic :: Bool
                                     , addedSugar :: Bool
                                     , vegan :: Bool
                                     } deriving (Eq, Show, Generic)
  instance ToJSON PastOrderItem