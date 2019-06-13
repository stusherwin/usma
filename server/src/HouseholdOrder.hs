{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import HouseholdOrderItem (HouseholdOrderItem)
  
  data HouseholdOrder = HouseholdOrder { orderId :: Int
                                       , orderCreatedDate :: UTCTime
                                       , orderCreatedBy :: Int
                                       , orderCreatedByName :: String
                                       , householdId :: Int
                                       , householdName :: String 
                                       , isComplete :: Bool
                                       , isAbandoned :: Bool
                                       , isOpen :: Bool
                                       , status :: HouseholdOrderStatus
                                       , oldTotalExcVat :: Maybe Int
                                       , oldTotalIncVat :: Maybe Int
                                       , totalExcVat :: Int
                                       , totalIncVat :: Int
                                       , items :: [HouseholdOrderItem]
                                       } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrder

  data HouseholdOrderStatus = Open | Complete | Abandoned deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderStatus
      
  data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidQuantity :: Maybe Int
                                                             } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdOrderItemDetails
  instance FromJSON HouseholdOrderItemDetails

  householdOrder :: Int -> UTCTime -> Int -> String -> Int -> String -> Bool -> Bool -> Maybe Int -> Maybe Int -> Int -> Int -> [HouseholdOrderItem] -> HouseholdOrder
  householdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled totalExcVat totalIncVat newTotalExcVat newTotalIncVat items = 
    HouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled open status totalExcVat totalIncVat newTotalExcVat newTotalIncVat items 
    where
    open = not complete && not cancelled
    status = case (complete, cancelled) of
           (_, True) -> Abandoned
           (True, _) -> Complete
           _ -> Open