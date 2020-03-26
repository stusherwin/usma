{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment(..))
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , orderCreatedDate :: UTCTime
                                         , orderCreatedBy :: Maybe Int
                                         , orderCreatedByName :: Maybe String
                                         , orderIsPlaced :: Bool
                                         , orderIsAbandoned :: Bool
                                         , isComplete :: Bool
                                         , totalExcVat :: Int
                                         , totalIncVat :: Int
                                         , allHouseholdsUpToDate :: Bool
                                         , adjustment :: Maybe OrderAdjustment
                                         , items :: [OrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  collectiveOrder :: Int -> UTCTime -> Maybe Int -> Maybe String -> Bool -> Int -> Int -> Int -> Int -> Bool -> [OrderItem] -> CollectiveOrder
  collectiveOrder id createdDate createdBy createdByName complete _ _ totalExcVat totalIncVat True items = 
    CollectiveOrder id createdDate createdBy createdByName False False complete totalExcVat totalIncVat True Nothing items

  collectiveOrder id createdDate createdBy createdByName complete oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat False items = 
    CollectiveOrder id createdDate createdBy createdByName False False complete totalExcVat totalIncVat False (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items