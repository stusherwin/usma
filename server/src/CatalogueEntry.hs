{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CatalogueEntry where
  import Data.Aeson
  import GHC.Generics
  import Product

  data CatalogueEntry = CatalogueEntry { code :: String
                                       , category :: String
                                       , brand :: String
                                       , description :: String
                                       , text :: String
                                       , size :: String
                                       , price :: Int
                                       , vatRate :: VatRate
                                       , rrp :: Maybe Int
                                       , biodynamic :: Bool
                                       , fairTrade :: Bool
                                       , glutenFree :: Bool
                                       , organic :: Bool
                                       , addedSugar :: Bool
                                       , vegan :: Bool
                                       , priceChanged :: Maybe Date
                                       } deriving (Eq, Show, Generic)
  instance ToJSON CatalogueEntry
