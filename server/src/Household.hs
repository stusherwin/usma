{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Household where
  import Data.Aeson
  import GHC.Generics

  data Household = Household { id :: Int
                             , name :: String
                             } deriving (Eq, Show, Generic)
  instance ToJSON Household