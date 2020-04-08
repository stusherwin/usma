{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Function (on)
import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (groupBy, maximumBy)
import Data.Maybe (isJust, maybe, fromMaybe)
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE (fromList)
import GHC.Generics
import Prelude hiding (product)

{-- OrderGroup --}

newtype OrderGroupId = OrderGroupId 
  { fromOrderGroupId :: Int 
  } deriving (Eq, Show, Generic)

{- Household -}

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

data Household = Household 
  { _householdInfo :: HouseholdInfo
  , _householdContactName :: Maybe String
  , _householdContactEmail :: Maybe String
  , _householdContactPhone :: Maybe String
  , _householdTotalOrders :: Int
  , _householdTotalPayments :: Int
  , _householdBalance :: Int
  }

household :: HouseholdInfo -> Maybe String -> Maybe String -> Maybe String -> [HouseholdOrder] -> [Payment] -> Household
household householdInfo contactName contactEmail contactPhone householdOrders payments =
  Household householdInfo contactName contactEmail contactPhone totalOrders totalPayments balance
  where
  totalOrders = _incVat $ sum . map (\ho -> case ho of 
    (HouseholdOrder { _householdOrderAdjustment = Just adj }) -> adj
    _ -> _householdOrderTotal ho) $ filter (not . isHouseholdOrderAbandoned) householdOrders
  totalPayments = sum . map _paymentAmount $ payments
  balance = totalPayments - totalOrders

{- Payment -}

newtype PaymentId = PaymentId 
  { fromPaymentId :: Int 
  } deriving (Eq, Show, Generic)

data Payment = Payment 
  { _paymentId :: PaymentId
  , _paymentHouseholdId :: HouseholdId
  , _paymentDate :: UTCTime
  , _paymentAmount :: Int
  } deriving (Eq, Show, Generic)

{-- Order --}

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
  , _orderTotal :: Money
  , _orderAdjustment :: Maybe OrderAdjustment
  , _orderItems :: [OrderItem]
  , _householdOrders :: [HouseholdOrder]
  } deriving (Eq, Show, Generic)

order :: OrderInfo -> Bool -> Bool -> [HouseholdOrder] -> Order
order info isAbandoned isPlaced householdOrders =
  Order info 
        (orderStatus isAbandoned isPlaced householdOrders)
        total
        adjustment
        items
        householdOrders
  where items = map (sconcat . NE.fromList) 
              . groupBy ((==) `on` (_productId . _itemProduct))
              . concatMap _householdOrderItems
              $ householdOrders
        adjustment = if total == adjTotal
                       then Nothing
                       else Just $ OrderAdjustment adjTotal 
        total    = sum . map _householdOrderTotal $ householdOrders
        adjTotal = sum . map adjHouseholdOrderTotal $ householdOrders
        adjHouseholdOrderTotal ho = fromMaybe (_householdOrderTotal ho) $ fmap _orderAdjNewTotal $ _householdOrderAdjustment ho

data OrderStatus = OrderAbandoned
                 | OrderComplete  ProductCatalogueUpdateStatus
                 | OrderPlaced    OrderReconcileStatus
                 | OrderOpen      ProductCatalogueUpdateStatus
  deriving (Eq, Show, Generic)

data ProductCatalogueUpdateStatus = NoUpdate
                                  | AwaitingConfirm
  deriving (Eq, Show, Generic)

data OrderReconcileStatus = NotReconciled
                          | Reconciled
  deriving (Eq, Show, Generic)

orderStatus :: Bool -> Bool -> [HouseholdOrder] -> OrderStatus
orderStatus True _    _ = OrderAbandoned
orderStatus _    True hos = OrderPlaced $ if all isHouseholdOrderReconciled hos 
                                            then Reconciled 
                                            else NotReconciled
orderStatus _    _    hos = let updateStatus = if any isHouseholdOrderAwaitingConfirm hos 
                                                  then AwaitingConfirm 
                                                  else NoUpdate
                            in  if all isHouseholdOrderComplete hos 
                                  then OrderComplete updateStatus 
                                  else OrderOpen updateStatus

orderIsAbandoned (Order { _orderStatus = OrderAbandoned }) = True
orderIsAbandoned _ = False

orderIsPlaced (Order { _orderStatus = OrderPlaced _ }) = True
orderIsPlaced _ = False

orderIsComplete (Order { _orderStatus = OrderComplete _ }) = True
orderIsComplete _ = False

orderIsAllHouseholdsUpToDate (Order { _orderStatus = OrderComplete NoUpdate }) = True
orderIsAllHouseholdsUpToDate (Order { _orderStatus = OrderOpen     NoUpdate }) = True
orderIsAllHouseholdsUpToDate _ = False

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Money
  } deriving (Eq, Show, Generic)

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatus :: HouseholdOrderStatus
  , _householdOrderUpdated :: UTCTime
  , _householdOrderItems :: [OrderItem]
  , _householdOrderTotal :: Money
  , _householdOrderAdjustment :: Maybe OrderAdjustment
  } deriving (Eq, Show, Generic)

householdOrder :: OrderInfo -> HouseholdInfo -> Bool -> Bool -> Bool -> UTCTime -> [OrderItem] -> HouseholdOrder
householdOrder orderInfo householdInfo isAbandoned isPlaced isComplete updated items 
  = HouseholdOrder orderInfo
                   householdInfo
                   (householdOrderStatus isAbandoned isPlaced isComplete updated items)
                   updated
                   items
                   total
                   adjustment
  where
  adjustment = if total == adjTotal 
                 then Nothing 
                 else Just $ OrderAdjustment adjTotal
  total    = sum . map _itemTotal $ items
  adjTotal = sum . map adjItemTotal $ items
  adjItemTotal i = fromMaybe (_itemTotal i) $ fmap _itemAdjNewTotal $ _itemAdjustment i

data HouseholdOrderStatus = HouseholdOrderAbandoned
                          | HouseholdOrderPlaced   OrderReconcileStatus
                          | HouseholdOrderComplete ProductCatalogueUpdateStatus
                          | HouseholdOrderOpen     ProductCatalogueUpdateStatus
  deriving (Eq, Show, Generic)

householdOrderStatus :: Bool -> Bool -> Bool -> UTCTime -> [OrderItem] -> HouseholdOrderStatus
householdOrderStatus True _    _    _       _  = HouseholdOrderAbandoned
householdOrderStatus _    True _    _       is = HouseholdOrderPlaced   $ if all (isJust . _itemAdjustment) is 
                                                                             then Reconciled
                                                                             else NotReconciled
householdOrderStatus _    _    True updated is = HouseholdOrderComplete $ if any ((> updated) . _productUpdated . _itemProduct) is 
                                                                             then AwaitingConfirm 
                                                                             else NoUpdate
householdOrderStatus _    _    _    updated is = HouseholdOrderOpen     $ if any ((> updated) . _productUpdated . _itemProduct) is 
                                                                             then AwaitingConfirm 
                                                                             else NoUpdate

isHouseholdOrderReconciled :: HouseholdOrder -> Bool
isHouseholdOrderReconciled (HouseholdOrder { _householdOrderStatus = HouseholdOrderPlaced Reconciled }) = True
isHouseholdOrderReconciled _ = False

isHouseholdOrderAwaitingConfirm :: HouseholdOrder -> Bool
isHouseholdOrderAwaitingConfirm (HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete AwaitingConfirm }) = True
isHouseholdOrderAwaitingConfirm (HouseholdOrder { _householdOrderStatus = HouseholdOrderOpen     AwaitingConfirm }) = True
isHouseholdOrderAwaitingConfirm _ = False

isHouseholdOrderComplete :: HouseholdOrder -> Bool
isHouseholdOrderComplete (HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete _ }) = True
isHouseholdOrderComplete _ = False

isHouseholdOrderAbandoned :: HouseholdOrder -> Bool
isHouseholdOrderAbandoned (HouseholdOrder { _householdOrderStatus = HouseholdOrderAbandoned }) = True
isHouseholdOrderAbandoned _ = False

{- OrderItem -}

data OrderItem = OrderItem 
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemTotal :: Money
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

orderItem :: Product -> Int -> Maybe OrderItemAdjustment -> OrderItem
orderItem product quantity adjustment = OrderItem product quantity (_productPrice product * fromIntegral quantity) adjustment

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem (_itemProduct    i1)
                       (_itemQuantity   i1 +  _itemQuantity   i2)
                       (_itemTotal      i1 +  _itemTotal      i2)
                       (_itemAdjustment i1 <> _itemAdjustment i2)

data OrderItemAdjustment = OrderItemAdjustment 
  { _itemAdjNewVatRate :: VatRate
  , _itemAdjNewPrice :: Money
  , _itemAdjNewQuantity :: Int
  , _itemAdjNewTotal :: Money
  , _itemAdjIsDiscontinued :: Bool
  , _itemAdjDate :: UTCTime
  } deriving (Eq, Show, Generic)

orderItemAdjustment :: VatRate -> Money -> Int -> Bool -> UTCTime -> OrderItemAdjustment
orderItemAdjustment newVatRate newPrice newQuantity isDiscontinued date
  = OrderItemAdjustment newVatRate newPrice newQuantity (newPrice * fromIntegral newQuantity) isDiscontinued date

instance Semigroup OrderItemAdjustment where
  a1 <> a2 = OrderItemAdjustment latestVatRate
                                 latestPrice
                                 totalQuantity
                                 total
                                 discontinued
                                 latestDate
    where 
    discontinued     = _itemAdjIsDiscontinued a1 || _itemAdjIsDiscontinued a2
    totalQuantity    = if discontinued
                         then 0 
                         else _itemAdjNewQuantity a1 + _itemAdjNewQuantity a2
    latest           = maximumBy (comparing _itemAdjDate) [a1, a2]
    latestPrice      = _itemAdjNewPrice latest
    latestVatRate    = _itemAdjNewVatRate latest
    latestDate       = _itemAdjDate latest
    total            = atNewVatRate latestVatRate $ latestPrice * fromIntegral totalQuantity

{- Product -}

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

data Product = Product 
  { _productId :: ProductId
  , _productCode :: String
  , _productName :: String
  , _productVatRate :: VatRate
  , _productPrice :: Money
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

{- Money -}

data Money = Money 
  { _excVat :: Int 
  , _incVat :: Int 
  } deriving (Eq, Ord, Show, Generic)

instance Num Money where
  Money exc1 inc1 + Money exc2 inc2 = Money (exc1 + exc2) (inc1 + inc2)
  Money exc1 inc1 - Money exc2 inc2 = Money (exc1 - exc2) (inc1 - inc2)
  Money exc1 inc1 * Money exc2 inc2 = Money (exc1 * exc2) (inc1 * inc2)
  abs (Money exc inc) = Money (abs exc) (abs inc)
  signum (Money exc inc) = Money (signum exc) (signum inc)
  fromInteger i = Money (fromInteger i) (fromInteger i)

{- VatRate -}

data VatRateType = Zero 
                 | Standard 
                 | Reduced 
  deriving (Eq, Show, Generic)

data VatRate = VatRate
  { _type :: VatRateType
  , _multiplier :: Rational
  } deriving (Eq, Show, Generic)

zeroRate :: VatRate
zeroRate = VatRate Zero 1

atVatRate :: VatRate -> Int -> Money
atVatRate vatRate amountExcVat = Money amountExcVat $ round $ fromIntegral amountExcVat * (_multiplier vatRate)

atNewVatRate :: VatRate -> Money -> Money
atNewVatRate vatRate = atVatRate vatRate . _excVat