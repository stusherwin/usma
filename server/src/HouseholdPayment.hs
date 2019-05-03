{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module HouseholdPayment where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Clock (UTCTime)
  
  data HouseholdPayment = HouseholdPayment { id :: Int
                                           , householdId :: Int
                                           , date :: UTCTime
                                           , amount :: Int
                                           } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdPayment

  data HouseholdPaymentDetails = HouseholdPaymentDetails { hpdDate :: UTCTime
                                                         , hpdAmount :: Int
                                                         } deriving (Eq, Show, Generic)
  instance ToJSON HouseholdPaymentDetails
  instance FromJSON HouseholdPaymentDetails