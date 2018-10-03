{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import Product (VatRate)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: Day
                                       , isOrderPast :: Bool
                                       , householdId :: Int
                                       , householdName :: String 
                                       , isComplete :: Bool
                                       , isCancelled :: Bool
                                       , isOpen :: Bool
                                       , status :: HouseholdOrderStatus
                                       , canBeAmended :: Bool
                                       , total :: Int
                                       , items :: [HouseholdOrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderItem = HouseholdOrderItem { productId :: Int
                                               , productCode :: String
                                               , productName :: String
                                               , productPrice :: Int
                                               , productVatRate :: VatRate
                                               , itemQuantity :: Int
                                               , itemTotal :: Int
                                               } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItem

  data HouseholdOrderStatus = Open | Complete | Cancelled | Placed deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderStatus
      
  data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidQuantity :: Int
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItemDetails
  instance FromJSON HouseholdOrderItemDetails

  householdOrder :: Int -> Day -> Bool -> Bool -> Int -> String -> Bool -> Bool -> Int -> [HouseholdOrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated orderPlaced orderPast householdId householdName complete cancelled total items = 
    HouseholdOrder orderId orderCreated orderPast householdId householdName complete cancelled open status canBeAmended total items 
    where
    open = not complete && not cancelled
    canBeAmended = not orderPlaced && not orderPast
    status = case (complete, cancelled, orderPlaced) of
           (_, _, True) -> Placed
           (_, True, _) -> Cancelled
           (True, _, _) -> Complete
           _ -> Open