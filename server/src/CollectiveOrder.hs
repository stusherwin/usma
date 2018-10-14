{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import OrderItem (OrderItem)
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , createdDate :: Day
                                         , isComplete :: Bool
                                         , status :: CollectiveOrderStatus
                                         , total :: Int
                                         , items :: [OrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderStatus = Open | Complete deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> Day -> Bool -> Int -> [OrderItem] -> CollectiveOrder
  collectiveOrder id created complete total items = 
    CollectiveOrder id created complete status total items
    where
    status = case complete of
               True -> Complete
               _ -> Open