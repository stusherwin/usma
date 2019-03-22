{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ProductCatalogueEntry where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate)

  data ProductCatalogueEntry = ProductCatalogueEntry { code :: String
                                                     , name :: String
                                                     , priceExcVat :: Int
                                                     , priceIncVat :: Int
                                                     , vatRate :: VatRate
                                                     } deriving (Eq, Show, Generic)
  instance ToJSON ProductCatalogueEntry
  instance FromJSON ProductCatalogueEntry
