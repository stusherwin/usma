{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Product where
  import Data.Aeson
  import GHC.Generics

  data Product = Product { id :: Int
                         , code :: String
                         , name :: String
                         , price :: Int
                         , vatRate :: VatRate
                         } deriving (Eq, Show, Generic)
  instance ToJSON Product

  data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
  instance ToJSON VatRate
  instance FromJSON VatRate
  
  data ProductDetails = ProductDetails { pdCode :: String
                                       , pdName :: String
                                       , pdPrice :: Int
                                       , pdVatRate :: VatRate
                                       } deriving (Eq, Show, Generic)
  instance ToJSON ProductDetails
  instance FromJSON ProductDetails
