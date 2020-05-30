{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Function (on)
import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (groupBy, maximumBy, find, delete)
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

data Household = Household 
  { _householdInfo :: HouseholdInfo
  , _householdContactName :: Maybe String
  , _householdContactEmail :: Maybe String
  , _householdContactPhone :: Maybe String
  , _householdTotalOrders :: Int
  , _householdTotalPayments :: Int
  , _householdBalance :: Int
  }

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

household :: HouseholdInfo -> Maybe String -> Maybe String -> Maybe String -> [HouseholdOrder] -> [Payment] -> Household
household householdInfo contactName contactEmail contactPhone householdOrders payments =
  Household householdInfo contactName contactEmail contactPhone totalOrders totalPayments balance
  where
  totalOrders = _moneyIncVat $ sum . map (\ho -> fromMaybe (_householdOrderTotal ho) (fmap _orderAdjNewTotal . _householdOrderAdjustment $ ho)) $ filter (not . isHouseholdOrderAbandoned) householdOrders
  totalPayments = sum . map _paymentAmount $ payments 
  balance = totalPayments - totalOrders

{- Payment -}

data Payment = Payment 
  { _paymentId :: PaymentId
  , _paymentHouseholdId :: HouseholdId
  , _paymentDate :: UTCTime
  , _paymentAmount :: Int
  } deriving (Eq, Show, Generic)

newtype PaymentId = PaymentId 
  { fromPaymentId :: Int 
  } deriving (Eq, Show, Generic)

{-- Order --}

data OrderSpec = OrderSpec 
  { _orderSpecCreated :: UTCTime
  , _orderSpecCreatedByHouseholdId :: Maybe HouseholdId
  }

data Order = Order 
  { _orderInfo :: OrderInfo
  , _orderStatus :: OrderStatus
  , _orderTotal :: Money
  , _orderAdjustment :: Maybe OrderAdjustment
  , _orderItems :: [OrderItem]
  , _householdOrders :: [HouseholdOrder]
  } deriving (Eq, Show, Generic)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Show, Generic)

data OrderInfo = OrderInfo
  { _orderId :: OrderId
  , _orderCreated :: UTCTime
  , _orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data OrderStatusFlags = OrderStatusFlags
  { _orderIsAbandoned :: Bool
  , _orderIsPlaced :: Bool
  } deriving (Eq, Show, Generic)

data OrderStatus = OrderAbandoned
                 | OrderComplete  (Maybe ProductCatalogueUpdateStatus)
                 | OrderPlaced    (Maybe OrderReconcileStatus)
                 | OrderOpen      (Maybe ProductCatalogueUpdateStatus)
                   deriving (Eq, Show, Generic)

data ProductCatalogueUpdateStatus = AwaitingConfirm
                                    deriving (Eq, Show, Generic)

data OrderReconcileStatus = Reconciled
                            deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Money
  } deriving (Eq, Show, Generic)

order :: OrderInfo -> OrderStatusFlags -> [HouseholdOrder] -> Order
order info statusFlags householdOrders =
  Order info 
        (status statusFlags householdOrders)
        total
        adjustment
        items
        householdOrders
  where items = map (sconcat . NE.fromList) 
              . groupBy ((==) `on` (_productId . _productInfo . _itemProduct))
              . concatMap _householdOrderItems
              $ householdOrders
        adjustment = if total == adjTotal
                       then Nothing
                       else Just $ OrderAdjustment adjTotal 
        total    = sum . map _householdOrderTotal   $ householdOrders
        adjTotal = sum . map adjHouseholdOrderTotal $ householdOrders
        adjHouseholdOrderTotal ho = fromMaybe (_householdOrderTotal ho) $ fmap _orderAdjNewTotal $ _householdOrderAdjustment ho
        status (OrderStatusFlags { _orderIsAbandoned = True }) _   = OrderAbandoned
        status (OrderStatusFlags { _orderIsPlaced    = True }) hos = OrderPlaced (reconcileStatus hos)
        status _ hos = (completeStatus hos) (updateStatus hos)
        reconcileStatus = justWhen Reconciled      . all isHouseholdOrderReconciled
        updateStatus    = justWhen AwaitingConfirm . any isHouseholdOrderAwaitingConfirm
        completeStatus hos = if all isHouseholdOrderComplete hos 
                               then OrderComplete
                               else OrderOpen

orderIsAbandoned (Order { _orderStatus = OrderAbandoned }) = True
orderIsAbandoned _ = False

orderIsPlaced (Order { _orderStatus = OrderPlaced _ }) = True
orderIsPlaced _ = False

orderIsComplete (Order { _orderStatus = OrderComplete _ }) = True
orderIsComplete _ = False

orderIsAllHouseholdsUpToDate (Order { _orderStatus = OrderComplete Nothing }) = True
orderIsAllHouseholdsUpToDate (Order { _orderStatus = OrderOpen     Nothing }) = True
orderIsAllHouseholdsUpToDate _ = False

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatus :: HouseholdOrderStatus
  , _householdOrderItems :: [OrderItem]
  , _householdOrderTotal :: Money
  , _householdOrderAdjustment :: Maybe OrderAdjustment
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatusFlags = HouseholdOrderStatusFlags
  { _householdOrderIsAbandoned :: Bool
  , _householdOrderIsPlaced :: Bool
  , _householdOrderIsComplete :: Bool
  , _householdOrderUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderAbandoned
                          | HouseholdOrderPlaced   (Maybe OrderReconcileStatus)
                          | HouseholdOrderComplete (Maybe ProductCatalogueUpdateStatus)
                          | HouseholdOrderOpen     (Maybe ProductCatalogueUpdateStatus)
                            deriving (Eq, Show, Generic)

householdOrder :: OrderInfo -> HouseholdInfo -> HouseholdOrderStatusFlags -> [OrderItem] -> HouseholdOrder
householdOrder orderInfo householdInfo statusFlags items 
  = HouseholdOrder orderInfo
                   householdInfo
                   (status statusFlags items)
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
  status (HouseholdOrderStatusFlags { _householdOrderIsAbandoned = True }) _  = HouseholdOrderAbandoned
  status (HouseholdOrderStatusFlags { _householdOrderIsPlaced    = True }) is = HouseholdOrderPlaced   $ reconcileStatus is
  status (HouseholdOrderStatusFlags { _householdOrderIsComplete  = True
                                    , _householdOrderUpdated = updated })  is = HouseholdOrderComplete $ updateStatus updated is
  status (HouseholdOrderStatusFlags { _householdOrderUpdated = updated })  is = HouseholdOrderOpen     $ updateStatus updated is 
  reconcileStatus      = justWhen Reconciled      . all (isJust . _itemAdjustment)
  updateStatus updated = justWhen AwaitingConfirm . any ((> updated) . _productUpdated . _productInfo . _itemProduct)

isHouseholdOrderReconciled :: HouseholdOrder -> Bool
isHouseholdOrderReconciled (HouseholdOrder { _householdOrderStatus = HouseholdOrderPlaced (Just Reconciled) }) = True
isHouseholdOrderReconciled _ = False

isHouseholdOrderAwaitingConfirm :: HouseholdOrder -> Bool
isHouseholdOrderAwaitingConfirm (HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete (Just AwaitingConfirm) }) = True
isHouseholdOrderAwaitingConfirm (HouseholdOrder { _householdOrderStatus = HouseholdOrderOpen     (Just AwaitingConfirm) }) = True
isHouseholdOrderAwaitingConfirm _ = False

isHouseholdOrderComplete :: HouseholdOrder -> Bool
isHouseholdOrderComplete (HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete _ }) = True
isHouseholdOrderComplete _ = False

isHouseholdOrderAbandoned :: HouseholdOrder -> Bool
isHouseholdOrderAbandoned (HouseholdOrder { _householdOrderStatus = HouseholdOrderAbandoned }) = True
isHouseholdOrderAbandoned _ = False

updateHouseholdOrderItem :: Product -> (Maybe Int) -> HouseholdOrder -> HouseholdOrder
updateHouseholdOrderItem product maybeQuantity order = 
    order { _householdOrderItems = items'
          , _householdOrderTotal = sum . map _itemTotal $ items'
          }
  where
    items = _householdOrderItems order
    (item', itemsWithoutItem) = case find ((== productCode) . itemProductCode) items of
      Just i -> (updateOrderItemQuantity (fromMaybe (_itemQuantity i) maybeQuantity) i, delete i items)
      _      -> (orderItem product 1 Nothing, items)                       
    items' = item':itemsWithoutItem
    productCode = _productCode . _productInfo $ product
    itemProductCode = _productCode . _productInfo . _itemProduct

{- OrderItem -}

data OrderItem = OrderItem  
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemTotal :: Money
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

orderItem :: Product -> Int -> Maybe OrderItemAdjustment -> OrderItem
orderItem product quantity adjustment = OrderItem product quantity ((_priceAmount . _productPrice . _productInfo $ product) * fromIntegral quantity) adjustment

updateOrderItemQuantity :: Int -> OrderItem -> OrderItem
updateOrderItemQuantity quantity item = item { _itemQuantity = quantity
                                             , _itemTotal = ((_priceAmount . _productPrice . _productInfo . _itemProduct $ item) * fromIntegral quantity)
                                             }

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem (_itemProduct    i1)
                       (_itemQuantity   i1 +  _itemQuantity   i2)
                       (_itemTotal      i1 +  _itemTotal      i2)
                       (_itemAdjustment i1 <> _itemAdjustment i2)

data OrderItemAdjustment = OrderItemAdjustment 
  { _itemAdjNewPrice :: Price
  , _itemAdjNewQuantity :: Int
  , _itemAdjNewTotal :: Money
  , _itemAdjIsDiscontinued :: Bool
  , _itemAdjDate :: UTCTime
  } deriving (Eq, Show, Generic)

orderItemAdjustment :: Price -> Int -> Bool -> UTCTime -> OrderItemAdjustment
orderItemAdjustment newPrice newQuantity isDiscontinued date
  = OrderItemAdjustment newPrice newQuantity (atQuantity newQuantity newPrice) isDiscontinued date

instance Semigroup OrderItemAdjustment where
  a1 <> a2 = OrderItemAdjustment latestPrice
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
    latestDate       = _itemAdjDate latest
    total            = atQuantity totalQuantity latestPrice

{- Product -}

newtype ProductId = ProductId 
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

data ProductInfo = ProductInfo
  { _productId :: ProductId
  , _productCode :: String
  , _productName :: String
  , _productPrice :: Price
  , _productUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

data Product = Product 
  { _productInfo :: ProductInfo
  , _productFlags :: ProductFlags
  } deriving (Eq, Show, Generic)

data ProductFlags = ProductFlags
  { _productIsBiodynamic :: Bool
  , _productIsFairTrade  :: Bool
  , _productIsGlutenFree :: Bool
  , _productIsOrganic    :: Bool
  , _productIsAddedSugar :: Bool
  , _productIsVegan      :: Bool
  } deriving (Eq, Show, Generic)

{- Price -}

data Price = Price
  { _priceVatRate :: VatRate
  , _priceAmount :: Money
  } deriving (Eq, Show, Generic)

atVatRate :: VatRate -> Int -> Price
atVatRate vatRate amountExcVat = Price vatRate $
  Money amountExcVat $ round $ fromIntegral amountExcVat * (_vatRateMultiplier  vatRate)

atQuantity :: Int -> Price -> Money
atQuantity quantity price = (_priceAmount price * fromIntegral quantity)

{- Money -}

data Money = Money 
  { _moneyExcVat :: Int 
  , _moneyIncVat :: Int 
  } deriving (Eq, Ord, Show, Generic)

instance Num Money where
  Money exc1 inc1 + Money exc2 inc2 = Money (exc1 + exc2) (inc1 + inc2)
  Money exc1 inc1 - Money exc2 inc2 = Money (exc1 - exc2) (inc1 - inc2)
  Money exc1 inc1 * Money exc2 inc2 = Money (exc1 * exc2) (inc1 * inc2)
  abs    (Money exc inc) = Money (abs exc) (abs inc)
  signum (Money exc inc) = Money (signum exc) (signum inc)
  fromInteger i          = Money (fromInteger i) (fromInteger i)

{- VatRate -}

data VatRateType = Zero 
                 | Standard 
                 | Reduced 
  deriving (Eq, Show, Generic)

data VatRate = VatRate
  { _vatRateType :: VatRateType
  , _vatRateMultiplier :: Rational
  } deriving (Eq, Show, Generic)

zeroRate :: VatRate
zeroRate = VatRate Zero 1

justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing