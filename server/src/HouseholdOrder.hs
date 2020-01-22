{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment(..))
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: UTCTime
                                       , orderCreatedBy :: Int
                                       , orderCreatedByName :: String
                                       , householdId :: Int
                                       , householdName :: String
                                       , isComplete :: Bool
                                       , isAbandoned :: Bool
                                       , isOpen :: Bool
                                       , totalExcVat :: Int
                                       , totalIncVat :: Int
                                       , adjustment :: Maybe OrderAdjustment
                                       , items :: [OrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidQuantity :: Maybe Int
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItemDetails
  instance FromJSON HouseholdOrderItemDetails

  householdOrder :: Int -> UTCTime -> Int -> String -> Int -> String -> Bool -> Bool -> Int -> Int -> Int -> Int -> Bool -> [OrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled _ _ totalExcVat totalIncVat False items = 
    HouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled open totalExcVat totalIncVat Nothing items 
    where
    open = not complete && not cancelled

  householdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat True items = 
    HouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled open totalExcVat totalIncVat (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items 
    where
    open = not complete && not cancelled