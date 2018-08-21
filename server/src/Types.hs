{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data Order = Order { oId :: Int
                     , oCreatedDate :: String
                     , oComplete :: Bool
                     , oCancelled :: Bool
                     , oTotal :: Int
                     } deriving (Eq, Show, Generic)
  instance ToJSON Order

  data OrderSummary = OrderSummary { osCreatedDate :: String
                                   , osComplete :: Bool
                                   , osCancelled :: Bool
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
                                                     , hosOrderComplete :: Bool
                                                     , hosHouseholdName :: String 
                                                     , hosCancelled :: Bool
                                                     , hosTotal :: Int
                                                     , hosItems :: [OrderSummary_Item]
                                                     } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderSummary

  data FullOrderSummary = FullOrderSummary { fosOrderCreatedDate :: String
                                           , fosComplete :: Bool
                                           , fosCancelled :: Bool
                                           , fosTotal :: Int
                                           , fosItems :: [OrderSummary_Item]
                                           } deriving (Eq, Show, Generic)
  instance ToJSON FullOrderSummary

  data OrderSummary_Item = OrderSummary_Item { osiProductId :: Int
                                             , osiProductName :: String
                                             , osiQuantity :: Int
                                             , osiTotal :: Int
                                             } deriving (Eq, Show, Generic)
  instance ToJSON OrderSummary_Item

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

  data CreateHousehold = CreateHousehold { chName :: String
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CreateHousehold
  instance FromJSON CreateHousehold

  data CreateProduct = CreateProduct { cpName :: String
                                     , cpPrice :: Int
                                     } deriving (Eq, Show, Generic)
  instance ToJSON CreateProduct
  instance FromJSON CreateProduct