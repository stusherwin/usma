{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , createdDate :: Day
                                         , isComplete :: Bool
                                         , isCancelled :: Bool
                                         , isPlaced :: Bool
                                         , isPast :: Bool
                                         , isPaid :: Bool
                                         , status :: CollectiveOrderStatus
                                         , canBeAmended :: Bool
                                         , total :: Int
                                         , items :: [CollectiveOrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderItem = CollectiveOrderItem { productId :: Int
                                                 , productName :: String
                                                 , itemQuantity :: Int
                                                 , itemTotal :: Int
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderItem

  data CollectiveOrderStatus = Open | Complete | Cancelled | Paid | Placed deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> Day -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> [CollectiveOrderItem] -> CollectiveOrder
  collectiveOrder id created complete cancelled placed past paid total items = 
    CollectiveOrder id created complete cancelled placed past paid status canBeAmended total items
    where
    canBeAmended = not placed && not past
    status = case (complete, cancelled, placed, paid) of
               (_, _, True, _) -> Placed
               (_, _, _, True) -> Paid
               (_, True, _, _) -> Cancelled
               (True, _, _, _) -> Complete
               _ -> Open