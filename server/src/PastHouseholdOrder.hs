{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastHouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import OrderItem (OrderItem, OrderAdjustment(..))
  
  data PastHouseholdOrder = PastHouseholdOrder { orderId :: Int
                                               , orderCreatedDate :: UTCTime
                                               , orderCreatedBy :: Int
                                               , orderCreatedByName :: String
                                               , orderIsPlaced :: Bool
                                               , orderIsAbandoned :: Bool
                                               , householdId :: Int
                                               , householdName :: String 
                                               , isAbandoned :: Bool
                                               , isComplete :: Bool
                                               , isOpen :: Bool
                                               , isReconciled :: Bool
                                               , totalExcVat :: Int
                                               , totalIncVat :: Int
                                               , adjustment :: Maybe OrderAdjustment
                                               , items :: [OrderItem]
                                               } deriving (Eq, Show, Generic)
  instance ToJSON PastHouseholdOrder

  pastHouseholdOrder :: Int -> UTCTime -> Int -> String -> Bool -> Int -> String -> Bool -> Bool -> Int -> Int -> Maybe Int -> Maybe Int -> [OrderItem] -> PastHouseholdOrder
  pastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName isOrderAbandoned householdId householdName isAbandoned isReconciled totalExcVat totalIncVat (Just oldTotalExcVat) (Just oldTotalIncVat) items =
    PastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName (not isOrderAbandoned) isOrderAbandoned householdId householdName isAbandoned (not isAbandoned) False isReconciled totalExcVat totalIncVat (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items
  
  pastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName isOrderAbandoned householdId householdName isAbandoned isReconciled totalExcVat totalIncVat _ _ items =
    PastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName (not isOrderAbandoned) isOrderAbandoned householdId householdName isAbandoned (not isAbandoned) False isReconciled totalExcVat totalIncVat Nothing items