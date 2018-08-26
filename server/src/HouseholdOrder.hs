{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: Day
                                       , isOrderPast :: Bool
                                       , householdId :: Int
                                       , householdName :: String 
                                       , isComplete :: Bool
                                       , isCancelled :: Bool
                                       , isOpen :: Bool
                                       , canBeAmended :: Bool
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

  householdOrder :: Int -> Day -> Bool -> Bool -> Int -> String -> Bool -> Bool -> Int -> [HouseholdOrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated orderPlaced orderPast householdId householdName complete cancelled total items = 
    HouseholdOrder orderId orderCreated orderPast householdId householdName complete cancelled open canBeAmended total items 
    where
    open = not complete && not cancelled
    canBeAmended = not orderPlaced && not orderPast