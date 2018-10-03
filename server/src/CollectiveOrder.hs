{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import Product (VatRate)
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , createdDate :: Day
                                         , isComplete :: Bool
                                         , isCancelled :: Bool
                                         , isPlaced :: Bool
                                         , isPast :: Bool
                                         , status :: CollectiveOrderStatus
                                         , canBeAmended :: Bool
                                         , total :: Int
                                         , items :: [CollectiveOrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderItem = CollectiveOrderItem { productId :: Int
                                                 , productCode :: String
                                                 , productName :: String
                                                 , productPrice :: Int
                                                 , productVatRate :: VatRate
                                                 , itemQuantity :: Int
                                                 , itemTotal :: Int
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderItem

  data CollectiveOrderStatus = Open | Complete | Cancelled | Placed deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> Day -> Bool -> Bool -> Bool -> Bool -> Int -> [CollectiveOrderItem] -> CollectiveOrder
  collectiveOrder id created complete cancelled placed past total items = 
    CollectiveOrder id created complete cancelled placed past status canBeAmended total items
    where
    canBeAmended = not placed && not past
    status = case (complete, cancelled, placed) of
               (_, _, True) -> Placed
               (_, True, _) -> Cancelled
               (True, _, _) -> Complete
               _ -> Open