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
                                         , totalExcVat :: Int
                                         , totalIncVat :: Int
                                         , items :: [OrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderStatus = Open | Complete deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> Day -> Bool -> Int -> Int -> [OrderItem] -> CollectiveOrder
  collectiveOrder id created complete totalExcVat totalIncVat items = 
    CollectiveOrder id created complete status totalExcVat totalIncVat items
    where
    status = case complete of
               True -> Complete
               _ -> Open