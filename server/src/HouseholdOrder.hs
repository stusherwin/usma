{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import OrderItem (OrderItem)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: Day
                                       , householdId :: Int
                                       , householdName :: String 
                                       , isComplete :: Bool
                                       , isCancelled :: Bool
                                       , isOpen :: Bool
                                       , status :: HouseholdOrderStatus
                                       , total :: Int
                                       , items :: [OrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderStatus = Open | Complete | Cancelled deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderStatus
      
  data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidQuantity :: Int
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItemDetails
  instance FromJSON HouseholdOrderItemDetails

  householdOrder :: Int -> Day -> Int -> String -> Bool -> Bool -> Int -> [OrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated householdId householdName complete cancelled total items = 
    HouseholdOrder orderId orderCreated householdId householdName complete cancelled open status total items 
    where
    open = not complete && not cancelled
    status = case (complete, cancelled) of
           (_, True) -> Cancelled
           (True, _) -> Complete
           _ -> Open