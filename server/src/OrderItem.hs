{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module OrderItem where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate(..))

  data OrderItem = OrderItem { productId :: Int
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
                             } deriving (Eq, Show, Generic)
  instance ToJSON OrderItem