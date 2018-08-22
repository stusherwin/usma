{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: Day
                                       , isOrderComplete :: Bool
                                       , isOrderPast :: Bool
                                       , householdId :: Int
                                       , householdName :: String 
                                       , isCancelled :: Bool
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