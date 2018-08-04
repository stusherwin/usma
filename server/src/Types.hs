{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data Order = Order { oId :: String
                     , oCreatedDate :: String
                     , oComplete :: Bool
                     , oTotal :: Int
                     } deriving (Eq, Show, Generic)
  instance ToJSON Order

  data OrderSummary = OrderSummary { osId :: String
                                   , osCreatedDate :: String
                                   , osComplete :: Bool
                                   , osTotal :: Int
                                   , osHouseholds :: [OrderSummary_Household]
                                   } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary

  data OrderSummary_Household = OrderSummary_Household { oshId :: Int
                                                       , oshName :: String
                                                       , oshTotal :: Int
                                                       , oshStatus :: String --OrderSummary_Status
                                                       } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Household

  data OrderSummary_Status = Paid | Unpaid | Cancelled  deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Status

  data HouseholdOrderSummary = HouseholdOrderSummary { hosOrderId :: String
                                                     , hosOrderCreatedDate :: String
                                                     , hosHouseholdId :: Int
                                                     , hosHouseholdName :: String 
                                                     , hosPaid :: Bool
                                                     , hosCancelled :: Bool
                                                     , hosTotal :: Int
                                                     , hosItems :: [HouseholdOrderSummary_Item]
                                                     } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderSummary

  data HouseholdOrderSummary_Item = HouseholdOrderSummary_Item { hosiProductId :: Int
                                                               , hosiProductName :: String
                                                               , hosiQuantity :: Int
                                                               , hosiTotal :: Int
                                                               } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderSummary_Item