{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  
  data EnsureHouseholdOrderItem = EnsureHouseholdOrderItem { ehoiOrderId :: Int
                                                           , ehoiHouseholdId :: Int
                                                           , ehoiProductId :: Int 
                                                           , ehoiQuantity :: Int
                                                           } deriving (Eq, Show, Generic)
  instance ToJSON EnsureHouseholdOrderItem
  instance FromJSON EnsureHouseholdOrderItem
                                                                 
  data RemoveHouseholdOrderItem = RemoveHouseholdOrderItem { rhoiOrderId :: Int
                                                           , rhoiHouseholdId :: Int
                                                           , rhoiProductId :: Int 
                                                           } deriving (Eq, Show, Generic)
  instance ToJSON RemoveHouseholdOrderItem
  instance FromJSON RemoveHouseholdOrderItem
                                                                 
  data CancelHouseholdOrder = CancelHouseholdOrder { choOrderId :: Int
                                                   , choHouseholdId :: Int
                                                   } deriving (Eq, Show, Generic)
  instance ToJSON CancelHouseholdOrder
  instance FromJSON CancelHouseholdOrder

  data CreateHousehold = CreateHousehold { chName :: String
                                         } deriving (Eq, Show, Generic)
  instance ToJSON CreateHousehold
  instance FromJSON CreateHousehold

  data CreateProduct = CreateProduct { cpName :: String
                                     , cpPrice :: Int
                                     } deriving (Eq, Show, Generic)
  instance ToJSON CreateProduct
  instance FromJSON CreateProduct

  data UpdateProduct = UpdateProduct { upId :: Int
                                     , upName :: String
                                     , upPrice :: Int
                                     } deriving (Eq, Show, Generic)
  instance ToJSON UpdateProduct
  instance FromJSON UpdateProduct

  data CreateHouseholdPayment = CreateHouseholdPayment { chpHouseholdId :: Int
                                                       , chpDate :: Day
                                                       , chpAmount :: Int
                                                       } deriving (Eq, Show, Generic)
  instance ToJSON CreateHouseholdPayment
  instance FromJSON CreateHouseholdPayment