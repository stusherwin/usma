{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ProductCatalogueData where
  import Data.Aeson
  import Data.Time.Clock (UTCTime)
  import GHC.Generics
  import Product (VatRate(..))

  data ProductCatalogueData = ProductCatalogueData { code :: String
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
                                                   , updated :: UTCTime
                                                   } deriving (Eq, Show, Generic)
  instance ToJSON ProductCatalogueData
