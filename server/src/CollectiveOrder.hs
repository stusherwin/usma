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
                                         , oldTotalExcVat :: Maybe Int
                                         , oldTotalIncVat :: Maybe Int
                                         , totalExcVat :: Int
                                         , totalIncVat :: Int
                                         , allHouseholdsUpToDate :: Bool
                                         , items :: [OrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderStatus = Open | Complete deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderStatus

  collectiveOrder :: Int -> UTCTime -> Int -> String -> Bool -> Maybe Int -> Maybe Int -> Int -> Int -> Bool -> [OrderItem] -> CollectiveOrder
  collectiveOrder id createdDate createdBy createdByName complete totalExcVat totalIncVat newTotalExcVat newTotalIncVat allHouseholdsUpToDate items = 
    CollectiveOrder id createdDate createdBy createdByName complete status totalExcVat totalIncVat newTotalExcVat newTotalIncVat allHouseholdsUpToDate items
    where
    status = case complete of
               True -> Complete
               _ -> Open