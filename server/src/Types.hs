{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (toGregorian, fromGregorian, Day )
  
  data Order = Order { oId :: Int
                     , oDate :: Date
                     } deriving (Eq, Show, Generic)
  instance ToJSON Order

  data Date = Date { year :: Int
                   , month :: Int
                   , day :: Int
                   } deriving (Eq, Ord, Show, Generic)
  instance ToJSON Date

  toDate :: Day -> Date
  toDate day = let (y, m, d) = toGregorian day
                    in Date (fromInteger y) m d

  fromDate :: Date -> Day
  fromDate d = fromGregorian (toInteger $ year d) (month d) (day d)