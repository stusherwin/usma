{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem)
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , createdDate :: UTCTime
                                         , createdBy :: Int
                                         , createdByName :: String
                                         , isComplete :: Bool
                                         , status :: CollectiveOrderStatus
                                         , totalExcVat :: Int
                                         , totalIncVat :: Int
                                         , items :: [OrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderStatus = Open | Complete deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> UTCTime -> Int -> String -> Bool -> Int -> Int -> [OrderItem] -> CollectiveOrder
  collectiveOrder id createdDate createdBy createdByName complete totalExcVat totalIncVat items = 
    CollectiveOrder id createdDate createdBy createdByName complete status totalExcVat totalIncVat items
    where
    status = case complete of
               True -> Complete
               _ -> Open