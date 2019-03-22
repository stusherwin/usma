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
                                       , isAbandoned :: Bool
                                       , isOpen :: Bool
                                       , status :: HouseholdOrderStatus
                                       , totalExcVat :: Int
                                       , totalIncVat :: Int
                                       , items :: [OrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderStatus = Open | Complete | Abandoned deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderStatus
      
  data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidQuantity :: Int
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItemDetails
  instance FromJSON HouseholdOrderItemDetails

  householdOrder :: Int -> Day -> Int -> String -> Bool -> Bool -> Int -> Int -> [OrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated householdId householdName complete cancelled totalExcVat totalIncVat items = 
    HouseholdOrder orderId orderCreated householdId householdName complete cancelled open status totalExcVat totalIncVat items 
    where
    open = not complete && not cancelled
    status = case (complete, cancelled) of
           (_, True) -> Abandoned
           (True, _) -> Complete
           _ -> Open