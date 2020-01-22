{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastHouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment)
  
  data PastHouseholdOrder = PastHouseholdOrder { orderId :: Int
                                               , orderCreatedDate :: UTCTime
                                               , orderCreatedBy :: Int
                                               , orderCreatedByName :: String
                                               , householdId :: Int
                                               , householdName :: String 
                                               , isAbandoned :: Bool
                                               , isComplete :: Bool
                                               , isPlaced :: Bool
                                               , totalExcVat :: Int
                                               , totalIncVat :: Int
                                               , adjustment :: Maybe OrderAdjustment
                                               , items :: [OrderItem]
                                               } deriving (Eq, Show, Generic)
  instance ToJSON PastHouseholdOrder

  pastHouseholdOrder :: Int -> UTCTime -> Int -> String -> Int -> String -> Bool -> Int -> Int -> [OrderItem] -> PastHouseholdOrder
  pastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName householdId householdName isAbandoned totalExcVat totalIncVat items =
    PastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName householdId householdName isAbandoned (not isAbandoned) (not isAbandoned) totalExcVat totalIncVat Nothing items