{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CollectiveOrder where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data CollectiveOrder = CollectiveOrder { id :: Int
                                         , createdDate :: Day
                                         , complete :: Bool
                                         , cancelled :: Bool
                                         , total :: Int
                                         , householdIds :: [Int]
                                         , items :: [CollectiveOrderItem]
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrder

  data CollectiveOrderItem = CollectiveOrderItem { productId :: Int
                                                 , productName :: String
                                                 , itemQuantity :: Int
                                                 , itemTotal :: Int
                                                 } deriving (Eq, Show, Generic)
  instance ToJSON CollectiveOrderItem