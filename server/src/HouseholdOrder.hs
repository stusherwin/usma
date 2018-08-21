{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: Day
                                       , orderComplete :: Bool
                                       , householdId :: Int
                                       , householdName :: String 
                                       , cancelled :: Bool
                                       , total :: Int
                                       , items :: [HouseholdOrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderItem = HouseholdOrderItem { productId :: Int
                                               , productName :: String
                                               , itemQuantity :: Int
                                               , itemTotal :: Int
                                               } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItem