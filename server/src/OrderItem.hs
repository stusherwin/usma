{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module OrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate)
  
  data OrderItem = OrderItem { productId :: Int
                             , productCode :: String
                             , productName :: String
                             , productPriceExcVat :: Int
                             , productPriceIncVat :: Int
                             , productVatRate :: VatRate
                             , itemQuantity :: Int
                             , itemTotalExcVat :: Int
                             , itemTotalIncVat :: Int
                             } deriving (Eq, Show, Generic)
  instance ToJSON OrderItem