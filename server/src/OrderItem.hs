{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module OrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate)
  
  data OrderItem = OrderItem { productId :: Int
                             , productCode :: String
                             , productName :: String
                             , productPrice :: Int
                             , productVatRate :: VatRate
                             , itemQuantity :: Int
                             , itemTotal :: Int
                             } deriving (Eq, Show, Generic)
  instance ToJSON OrderItem