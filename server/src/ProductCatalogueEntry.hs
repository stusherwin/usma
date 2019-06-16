{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ProductCatalogueEntry where
  import Data.Aeson
  import GHC.Generics
  import Product (VatRate(..))

  data ProductCatalogueEntry = ProductCatalogueEntry { code :: String
                                                     , name :: String
                                                     , priceExcVat :: Int
                                                     , priceIncVat :: Int
                                                     , vatRate :: VatRate
                                                     , biodynamic :: Bool
                                                     , fairTrade :: Bool
                                                     , glutenFree :: Bool
                                                     , organic :: Bool
                                                     , addedSugar :: Bool
                                                     , vegan :: Bool
                                                     } deriving (Eq, Show, Generic)
  instance ToJSON ProductCatalogueEntry
  instance FromJSON ProductCatalogueEntry
