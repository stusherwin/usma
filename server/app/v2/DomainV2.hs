{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (lookup)
import Data.Foldable (foldl')
import GHC.Generics
import Prelude hiding (sum)

{-- Order --}

data OrderStatus = OrderOpen
                 | OrderComplete
                 | OrderAwaitingHouseholdsUpdateConfirm
                 | OrderPlaced
                 | OrderReconciled
                 | OrderAbandoned
  deriving (Eq, Show, Generic)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Show, Generic)

data OrderInfo = OrderInfo
  { orderId :: OrderId
  , orderCreated :: UTCTime
  , orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data Order = Order 
  { orderInfo :: OrderInfo
  , orderStatus :: OrderStatus
  -- , orderIsPlaced :: Bool
  -- , orderIsAbandoned :: Bool
  -- , orderIsComplete :: Bool
  , orderTotal :: Value
  -- , orderIsAllHouseholdsUpToDate :: Bool
  , orderAdjustment :: Maybe OrderAdjustment
  , orderItems :: [OrderItem]
  , householdOrders :: [HouseholdOrder]
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

order :: OrderInfo -> Bool -> Bool -> [OrderItem] -> [HouseholdOrder] -> Order
order info isPlaced isAbandoned items householdOrders =
  Order info 
        (status isPlaced isAbandoned householdOrders)
        (total items)
        (adjustment items householdOrders)
        items
        householdOrders
  where status True _ _  = OrderPlaced
        status _ True _  = OrderAbandoned
        status _ _ items = undefined 
        total = (sum . map itemTotal)
        adjustment = undefined

{- Household -}

data HouseholdInfo = HouseholdInfo 
  { householdId :: HouseholdId
  , householdName :: String
  } deriving (Eq, Show, Generic)

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { householdOrderOrderInfo :: OrderInfo
  , householdOrderHouseholdInfo :: HouseholdInfo
  , householdOrderStatus :: HouseholdOrderStatus
  , householdOrderUpdated :: UTCTime
  , householdOrderItems :: [OrderItem]
  , householdOrderTotal :: Value
  , householdOrderAdjustment :: Maybe OrderAdjustment
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderOpen
                          | HouseholdOrderComplete
                          | HouseholdOrderAwaitingUpdateConfirm
                          | HouseholdOrderReconciled
                          | HouseholdOrderAbandoned
  deriving (Eq, Show, Generic)

{- Product -}

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

data Product = Product 
  { productId :: ProductId
  , productCode :: String
  , productName :: String
  , productVatRate :: VatRate
  , productPrice :: Value
  , productAttributes :: ProductAttributes
  } deriving (Eq, Show, Generic)

data ProductAttributes = ProductAttributes
  { productAttrIsBiodynamic :: Bool
  , productAttrIsFairTrade  :: Bool
  , productAttrIsGlutenFree :: Bool
  , productAttrIsOrganic    :: Bool
  , productAttrIsAddedSugar :: Bool
  , productAttrIsVegan      :: Bool
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

zero :: Value
zero = Value 0 0

sum :: [Value] -> Value
sum = foldl' (<>) zero

{- VatRate -}

data VatRate = Zero 
             | Standard 
             | Reduced 
  deriving (Eq, Show, Generic)

type VatRates = [(VatRate, Rational)]

atVatRate :: VatRates -> VatRate -> Int -> Value
atVatRate vatRates vatRate amount = 
  case lookup vatRate vatRates of
    Just multiplier -> Value amount $ round $ fromIntegral amount * multiplier
    _               -> Value amount amount