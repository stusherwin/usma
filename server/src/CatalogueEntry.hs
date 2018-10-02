{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CatalogueEntry where
  import Data.Aeson
  import Data.Time.Calendar (Day)
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
                                       , priceChanged :: Maybe Day
                                       } deriving (Eq, Show, Generic)
  instance ToJSON CatalogueEntry
