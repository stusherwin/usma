{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastCollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import OrderItem (OrderItem)

  data PastCollectiveOrder = PastCollectiveOrder { id :: Int
                                                 , createdDate :: Day
                                                 , createdBy :: Int
                                                 , createdByName :: String
                                                 , isAbandoned :: Bool
                                                 , totalExcVat :: Int
                                                 , totalIncVat :: Int
                                                 , items :: [OrderItem]
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON PastCollectiveOrder

  pastCollectiveOrder :: Int -> Day -> Int -> String -> Bool -> Int -> Int -> [OrderItem] -> PastCollectiveOrder
  pastCollectiveOrder = PastCollectiveOrder