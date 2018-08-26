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
                                       , householdBalance :: Int
                                       , isComplete :: Bool
                                       , isCancelled :: Bool
                                       , isOpen :: Bool
                                       , isPaid :: Bool
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

  householdOrder :: Int -> Day -> Bool -> Bool -> Int -> String -> Int -> Bool -> Bool -> Int -> [HouseholdOrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated orderPlaced orderPast householdId householdName householdBalance complete cancelled total items = 
    HouseholdOrder orderId orderCreated orderPast householdId householdName householdBalance complete cancelled open paid canBeAmended total items 
    where
    paid = householdBalance > 0
    open = not complete && not cancelled
    canBeAmended = not orderPlaced && not orderPast