{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data Order = Order { oId :: Int
                     , oCreatedDate :: String
                     , oComplete :: Bool
                     , oTotal :: Int
                     } deriving (Eq, Show, Generic)
  instance ToJSON Order

  data Product = Product { pId :: Int
                         , pName :: String
                         , pPrice :: Int
                         } deriving (Eq, Show, Generic)
  instance ToJSON Product

  data Household = Household { hId :: Int
                             , hName :: String
                             } deriving (Eq, Show, Generic)
  instance ToJSON Household

  data OrderSummary = OrderSummary { osCreatedDate :: String
                                   , osComplete :: Bool
                                   , osTotal :: Int
                                   , osHouseholds :: [OrderSummary_Household]
                                   } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary

  data OrderSummary_Household = OrderSummary_Household { oshId :: Int
                                                       , oshName :: String
                                                       , oshCancelled :: Bool
                                                       , oshTotal :: Int
                                                       } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Household

  data HouseholdOrderSummary = HouseholdOrderSummary { hosOrderCreatedDate :: String
                                                     , hosHouseholdName :: String 
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

  data EnsureHouseholdOrderItem = EnsureHouseholdOrderItem { ehoiOrderId :: Int
                                                           , ehoiHouseholdId :: Int
                                                           , ehoiProductId :: Int 
                                                           , ehoiQuantity :: Int
                                                           } deriving (Eq, Show, Generic)
  instance ToJSON EnsureHouseholdOrderItem
  instance FromJSON EnsureHouseholdOrderItem
                                                                 
  data RemoveHouseholdOrderItem = RemoveHouseholdOrderItem { rhoiOrderId :: Int
                                                           , rhoiHouseholdId :: Int
                                                           , rhoiProductId :: Int 
                                                           } deriving (Eq, Show, Generic)
  instance ToJSON RemoveHouseholdOrderItem
  instance FromJSON RemoveHouseholdOrderItem
                                                                 
  data CancelHouseholdOrder = CancelHouseholdOrder { choOrderId :: Int
                                                   , choHouseholdId :: Int
                                                   } deriving (Eq, Show, Generic)
  instance ToJSON CancelHouseholdOrder
  instance FromJSON CancelHouseholdOrder
