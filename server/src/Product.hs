{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Product where
  import Data.Time.Calendar (Day)
  import Data.Aeson
  import GHC.Generics

  data Product = Product { id :: Int
                         , code :: String
                         , name :: String
                         , price :: Int
                         , vatRate :: VatRate
                         , priceUpdated :: Maybe Day
                         } deriving (Eq, Show, Generic)
  instance ToJSON Product

  data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
  instance ToJSON VatRate
  instance FromJSON VatRate