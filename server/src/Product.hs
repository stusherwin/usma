{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Product where
  import Data.Aeson
  import GHC.Generics

  data Product = Product { id :: Int
                         , name :: String
                         , price :: Int
                         } deriving (Eq, Show, Generic)
  instance ToJSON Product