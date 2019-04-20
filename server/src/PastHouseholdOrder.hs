{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastHouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import OrderItem (OrderItem)
  
  data PastHouseholdOrder = PastHouseholdOrder { orderId :: Int
                                               , orderCreatedDate :: Day
                                               , orderCreatedBy :: Int
                                               , orderCreatedByName :: String
                                               , householdId :: Int
                                               , householdName :: String 
                                               , isAbandoned :: Bool
                                               , totalExcVat :: Int
                                               , totalIncVat :: Int
                                               , items :: [OrderItem]
                                               } deriving (Eq, Show, Generic)
  instance ToJSON PastHouseholdOrder

  pastHouseholdOrder :: Int -> Day -> Int -> String -> Int -> String -> Bool -> Int -> Int -> [OrderItem] -> PastHouseholdOrder
  pastHouseholdOrder = PastHouseholdOrder