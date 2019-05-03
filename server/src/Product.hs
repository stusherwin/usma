{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Product where
  import Data.Aeson
  import GHC.Generics

  data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
  instance ToJSON VatRate
  instance FromJSON VatRate