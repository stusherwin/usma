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
                                     } deriving (Eq, Show, Generic)
  instance ToJSON PastOrderItem