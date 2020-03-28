{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Time.Clock (UTCTime)
import Data.List (lookup)

{-- Order --}

data Order = Order 
  { orderId :: OrderId
  , created :: UTCTime
  , createdBy :: Maybe HouseholdInfo
  , isPlaced :: Bool
  , isAbandoned :: Bool
  , isComplete :: Bool
  , total :: Money
  , isAllHouseholdsUpToDate :: Bool
  , adjustment :: Maybe OrderAdjustment
  , items :: [OrderItem]
  } deriving (Eq, Show)

data OrderAdjustment = OrderAdjustment 
  { newTotal :: Money
  } deriving (Eq, Show)

data OrderItem = OrderItem 
  { product :: Product
  , quantity :: Int
  , total :: Money
  , adjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show)

data OrderItemAdjustment = OrderItemAdjustment 
  { newVatRate :: VatRate
  , newPrice :: Money
  , newQuantity :: Int
  , newTotal :: Money
  , isDiscontinued :: Bool
  } deriving (Eq, Show)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Show)

{- Household -}

data HouseholdInfo = HouseholdInfo 
  { householdId :: HouseholdId
  , name :: String
  } deriving (Eq, Show)

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show)

{- Product -}

data Product = Product 
  { productId :: ProductId
  , code :: String
  , name :: String
  , vatRate :: VatRate
  , price :: Money
  , isBiodynamic :: Bool
  , isFairTrade :: Bool
  , isGlutenFree :: Bool
  , isOrganic :: Bool
  , isAddedSugar :: Bool
  , isVegan :: Bool
  } deriving (Eq, Show)

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show)

{- Money -}

data Money = Money 
  { valueExcVat :: Int 
  , valueIncVat :: Int 
  } deriving (Eq, Show)

money :: VatRates -> VatRate -> Int -> Money
money vatRates vatRate amount = 
  case lookup vatRate vatRates of
    Just multiplier -> Money amount (round $ fromIntegral amount * multiplier)
    _               -> Money amount amount

zero :: Money
zero = Money 0 0

data VatRate = Zero 
             | Standard 
             | Reduced 
  deriving (Eq, Show)

type VatRates = [(VatRate, Double)]