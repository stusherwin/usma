{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastCollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment(..))

  data PastCollectiveOrder = PastCollectiveOrder { id :: Int
                                                 , createdDate :: UTCTime
                                                 , createdBy :: Int
                                                 , createdByName :: String
                                                 , isAbandoned :: Bool
                                                 , isComplete :: Bool
                                                 , isPlaced :: Bool
                                                 , isReconciled :: Bool
                                                 , totalExcVat :: Int
                                                 , totalIncVat :: Int
                                                 , allHouseholdsUpToDate :: Bool
                                                 , adjustment :: Maybe OrderAdjustment
                                                 , items :: [OrderItem]
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON PastCollectiveOrder

  pastCollectiveOrder :: Int -> UTCTime -> Int -> String -> Bool -> Bool -> Int -> Int -> Maybe Int -> Maybe Int -> [OrderItem] -> PastCollectiveOrder
  pastCollectiveOrder id createdDate createdBy createdByName isAbandoned isReconciled totalExcVat totalIncVat (Just oldTotalExcVat) (Just oldTotalIncVat) items = 
    PastCollectiveOrder id createdDate createdBy createdByName isAbandoned (not isAbandoned) (not isAbandoned) isReconciled totalExcVat totalIncVat True (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items
  
  pastCollectiveOrder id createdDate createdBy createdByName isAbandoned isReconciled totalExcVat totalIncVat _ _ items = 
    PastCollectiveOrder id createdDate createdBy createdByName isAbandoned (not isAbandoned) (not isAbandoned) isReconciled totalExcVat totalIncVat True Nothing items