{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastCollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment)

  data PastCollectiveOrder = PastCollectiveOrder { id :: Int
                                                 , createdDate :: UTCTime
                                                 , createdBy :: Int
                                                 , createdByName :: String
                                                 , isAbandoned :: Bool
                                                 , isComplete :: Bool
                                                 , totalExcVat :: Int
                                                 , totalIncVat :: Int
                                                 , adjustment :: Maybe OrderAdjustment
                                                 , items :: [OrderItem]
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON PastCollectiveOrder

  pastCollectiveOrder :: Int -> UTCTime -> Int -> String -> Bool -> Int -> Int -> [OrderItem] -> PastCollectiveOrder
  pastCollectiveOrder id createdDate createdBy createdByName isAbandoned totalExcVat totalIncVat items = 
    PastCollectiveOrder id createdDate createdBy createdByName isAbandoned (not isAbandoned) totalExcVat totalIncVat Nothing items