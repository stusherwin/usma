{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastHouseholdOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import PastOrderItem (PastOrderItem)
  
  data PastHouseholdOrder = PastHouseholdOrder { orderId :: Int
                                               , orderCreatedDate :: UTCTime
                                               , orderCreatedBy :: Int
                                               , orderCreatedByName :: String
                                               , householdId :: Int
                                               , householdName :: String 
                                               , isAbandoned :: Bool
                                               , totalExcVat :: Int
                                               , totalIncVat :: Int
                                               , items :: [PastOrderItem]
                                               } deriving (Eq, Show, Generic)
  instance ToJSON PastHouseholdOrder

  pastHouseholdOrder :: Int -> UTCTime -> Int -> String -> Int -> String -> Bool -> Int -> Int -> [PastOrderItem] -> PastHouseholdOrder
  pastHouseholdOrder = PastHouseholdOrder