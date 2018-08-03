{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data Order = Order { oId :: String
                     , oCreatedDate :: String
                     } deriving (Eq, Show, Generic)
  instance ToJSON Order

  data OrderSummary = OrderSummary { osId :: String
                                   , osCreatedDate :: String
                                   , osComplete :: Bool
                                   , osHouseholds :: [OrderSummary_Household]
                                   , osTotal :: Int
                                   } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary

  data OrderSummary_Household = OrderSummary_Household { oshId :: Int
                                                       , oshName :: String
                                                       , oshTotal :: Int
                                                       , oshStatus :: OrderSummary_Status
                                                       } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Household

  data OrderSummary_Status = Paid | Unpaid | Cancelled  deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Status