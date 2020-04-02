{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (lookup)
import Data.Foldable (foldl')
import GHC.Generics
import Prelude hiding (sum)

{-- OrderGroup --}

newtype OrderGroupId = OrderGroupId 
  { fromOrderGroupId :: Int 
  } deriving (Eq, Show, Generic)

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
  { _orderId :: OrderId
  , _orderCreated :: UTCTime
  , _orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data Order = Order 
  { _orderInfo :: OrderInfo
  , _orderStatus :: OrderStatus
  -- , orderIsPlaced :: Bool
  -- , orderIsAbandoned :: Bool
  -- , orderIsComplete :: Bool
  , _orderTotal :: Value
  -- , orderIsAllHouseholdsUpToDate :: Bool
  , _orderAdjustment :: Maybe OrderAdjustment
  , _orderItems :: [OrderItem]
  , _householdOrders :: [HouseholdOrder]
  } deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Value
  } deriving (Eq, Show, Generic)

data OrderItem = OrderItem 
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemTotal :: Value
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

instance Semigroup OrderItem where
  (OrderItem { _itemProduct = p1
             , _itemQuantity = q1
             , _itemTotal = t1
             , _itemAdjustment = a1 
             }) 
  <> 
  (OrderItem { _itemProduct = p2
             , _itemQuantity = q2
             , _itemTotal = t2
             , _itemAdjustment = a2 })
    = OrderItem p1 (q1 + q2) (t1 <> t2) $ case (a1, a2) of
                                             (Just a1, Just a2) -> a1 <> a2
                                             (Just a1, _)       -> a1
                                             (_, Just a2)       -> a2
                                             _                  -> Nothing

sumItems :: [OrderItem] -> OrderItem
sumItems = sconcat (<>)

data OrderItemAdjustment = OrderItemAdjustment 
  { --_itemAdjNewVatRate :: VatRate
    --, 
    _itemAdjNewPrice :: Value
  , _itemAdjNewQuantity :: Int
  , _itemAdjNewTotal :: Value
  , _itemAdjIsDiscontinued :: Bool
  } deriving (Eq, Show, Generic)

instance Semigroup OrderItemAdjustment where
  (OrderItemAdjustment { _itemAdjNewPrice = p1
                       , _itemAdjNewQuantity = q1
                       , _itemAdjNewTotal = t1
                       , _itemAdjIsDiscontinued = d1
                       })
  <>
  (OrderItemAdjustment { _itemAdjNewPrice = p2
                       , _itemAdjNewQuantity = q2
                       , _itemAdjNewTotal = t2
                       , _itemAdjIsDiscontinued = d2
                       })
  = OrderItemAdjustment (p1 <> p2) (q1 + q2) (t1 <> t2) (d1 || d2)


order :: OrderInfo -> Bool -> Bool -> {- [OrderItem] -> -} [HouseholdOrder] -> Order
order info isPlaced isAbandoned {- items -} householdOrders =
  Order info 
        (orderStatus isPlaced isAbandoned householdOrders)
        total
        (adjustment items householdOrders)
        items
        householdOrders
  where total = sum . map _householdOrderTotal $ householdOrders
        items = map foldl' sum $ groupBy ((==) on (_productId . _itemProduct)) $ concatMap _householdOrderItems householdOrders
        adjustment = undefined

orderStatus :: Bool -> Bool -> [HouseholdOrder] -> OrderStatus
orderStatus True _ _  = OrderPlaced
orderStatus _ True _  = OrderAbandoned
orderStatus _ _ householdOrders = undefined 

{- Household -}

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatus :: HouseholdOrderStatus
  , _householdOrderUpdated :: UTCTime
  , _householdOrderItems :: [OrderItem]
  , _householdOrderTotal :: Value
  , _householdOrderAdjustment :: Maybe OrderAdjustment
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderOpen
                          | HouseholdOrderComplete
                          | HouseholdOrderAwaitingUpdateConfirm
                          | HouseholdOrderReconciled
                          | HouseholdOrderAbandoned
  deriving (Eq, Show, Generic)

householdOrder :: OrderInfo -> HouseholdInfo -> OrderAdjustment -> [OrderItem] -> HouseholdOrder
householdOrder = undefined

{- Product -}

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

data Product = Product 
  { _productId :: ProductId
  , _productCode :: String
  , _productName :: String
  , _productVatRate :: VatRate
  , _productPrice :: Value
  , _productUpdated :: UTCTime
  , _productAttributes :: ProductAttributes
  } deriving (Eq, Show, Generic)

data ProductAttributes = ProductAttributes
  { _productAttrIsBiodynamic :: Bool
  , _productAttrIsFairTrade  :: Bool
  , _productAttrIsGlutenFree :: Bool
  , _productAttrIsOrganic    :: Bool
  , _productAttrIsAddedSugar :: Bool
  , _productAttrIsVegan      :: Bool
  } deriving (Eq, Show, Generic)

{- Value -}

data Value = Value 
  { _excVat :: Int 
  , _incVat :: Int 
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