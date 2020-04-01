{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (lookup)
import Data.Foldable (foldl')
import GHC.Generics

{-- Order --}

data Order = Order 
  { orderId :: OrderId
  , orderCreated :: UTCTime
  , orderCreatedBy :: Maybe HouseholdInfo
  , orderIsPlaced :: Bool
  , orderIsAbandoned :: Bool
  , orderIsComplete :: Bool
  , orderTotal :: Value
  , orderIsAllHouseholdsUpToDate :: Bool
  , orderAdjustment :: Maybe OrderAdjustment
  , orderItems :: [OrderItem]
  } deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { orderAdjNewTotal :: Value
  } deriving (Eq, Show, Generic)

data OrderItem = OrderItem 
  { itemProduct :: Product
  , itemQuantity :: Int
  , itemTotal :: Value
  , itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

data OrderItemAdjustment = OrderItemAdjustment 
  { itemAdjNewVatRate :: VatRate
  , itemAdjNewPrice :: Value
  , itemAdjNewQuantity :: Int
  , itemAdjNewTotal :: Value
  , itemAdjIsDiscontinued :: Bool
  } deriving (Eq, Show, Generic)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Show, Generic)

{- Household -}

data HouseholdInfo = HouseholdInfo 
  { householdId :: HouseholdId
  , householdName :: String
  } deriving (Eq, Show, Generic)

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

{- Product -}

data Product = Product 
  { productId :: ProductId
  , productCode :: String
  , productName :: String
  , productVatRate :: VatRate
  , productPrice :: Value
  , productIsBiodynamic :: Bool
  , productIsFairTrade :: Bool
  , productIsGlutenFree :: Bool
  , productIsOrganic :: Bool
  , productIsAddedSugar :: Bool
  , productIsVegan :: Bool
  } deriving (Eq, Show, Generic)

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

{- Value -}

data Value = Value 
  { excVat :: Int 
  , incVat :: Int 
  } deriving (Eq, Show, Generic)

instance Semigroup Value where
  Value exc1 inc1 <> Value exc2 inc2 = Value (exc1 + exc2) (inc1 + inc2)

instance Monoid Value where
  mempty = zero
  mappend = (<>)

value :: VatRates -> VatRate -> Int -> Value
value vatRates vatRate amount = 
  case lookup vatRate vatRates of
    Just multiplier -> Value amount $ round $ fromIntegral amount * multiplier
    _               -> Value amount amount

zero :: Value
zero = Value 0 0

sum :: [Value] -> Value
sum = foldl' (<>) zero

data VatRate = Zero 
             | Standard 
             | Reduced 
  deriving (Eq, Show, Generic)

type VatRates = [(VatRate, Rational)]