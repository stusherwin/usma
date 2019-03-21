{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastHouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import OrderItem (OrderItem)
  
  data PastHouseholdOrder = PastHouseholdOrder { orderId :: Int
                                               , orderCreatedDate :: Day
                                               , householdId :: Int
                                               , householdName :: String 
                                               , isAbandoned :: Bool
                                               , total :: Int
                                               , items :: [OrderItem]
                                               } deriving (Eq, Show, Generic)
  instance ToJSON PastHouseholdOrder

  pastHouseholdOrder :: Int -> Day -> Int -> String -> Bool -> Int -> [OrderItem] -> PastHouseholdOrder
  pastHouseholdOrder = PastHouseholdOrder