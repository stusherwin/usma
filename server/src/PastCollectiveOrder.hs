{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module PastCollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  import PastOrderItem (PastOrderItem)

  data PastCollectiveOrder = PastCollectiveOrder { id :: Int
                                                 , createdDate :: UTCTime
                                                 , createdBy :: Int
                                                 , createdByName :: String
                                                 , isAbandoned :: Bool
                                                 , totalExcVat :: Int
                                                 , totalIncVat :: Int
                                                 , items :: [PastOrderItem]
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON PastCollectiveOrder

  pastCollectiveOrder :: Int -> UTCTime -> Int -> String -> Bool -> Int -> Int -> [PastOrderItem] -> PastCollectiveOrder
  pastCollectiveOrder = PastCollectiveOrder