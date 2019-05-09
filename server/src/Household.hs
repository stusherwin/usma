{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Household where
  import Data.Aeson
  import GHC.Generics

  data Household = Household { id :: Int
                             , name :: String
                             , contactName :: Maybe String
                             , contactEmail :: Maybe String
                             , contactPhone :: Maybe String
                             , totalOrders :: Int
                             , totalPayments :: Int
                             , balance :: Int
                             } deriving (Eq, Show, Generic)
  instance ToJSON Household
                                                                   
  data HouseholdDetails = HouseholdDetails { hdName :: String
                                           , hdContactName :: Maybe String
                                           , hdContactEmail :: Maybe String
                                           , hdContactPhone :: Maybe String
                                           } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdDetails
  instance FromJSON HouseholdDetails