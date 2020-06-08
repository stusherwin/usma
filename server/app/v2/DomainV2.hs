{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import Data.Function (on)
import Data.Time.Clock (UTCTime)
import Data.Semigroup (Semigroup(..))
import Data.List (groupBy, maximumBy, find, delete)
import Data.Maybe (isJust, maybe, fromMaybe, catMaybes)
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE (fromList)
import GHC.Generics
import Prelude hiding (product)
import Text.Read (readMaybe)

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
  , _householdOrders :: [HouseholdOrder]
  , _householdPayments :: [Payment]
  }

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

household :: HouseholdInfo -> Maybe String -> Maybe String -> Maybe String -> [HouseholdOrder] -> [Payment] -> Household
household = Household

householdTotalOrders :: Household -> Int
householdTotalOrders = _moneyIncVat
                     . (sum . map (\ho -> fromMaybe (householdOrderTotal ho) (fmap _orderAdjNewTotal . householdOrderAdjustment $ ho)))
                     .  filter (not . isHouseholdOrderAbandoned) 
                     . _householdOrders

householdTotalPayments :: Household -> Int
householdTotalPayments = (sum . map _paymentAmount)
                       . _householdPayments 

householdBalance :: Household -> Int
householdBalance h = householdTotalPayments h - householdTotalOrders h

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
  , _orderHouseholdOrders :: [HouseholdOrder]
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
          householdOrders
  where 
    status (OrderStatusFlags { _orderIsAbandoned = True }) _   = OrderAbandoned
    status (OrderStatusFlags { _orderIsPlaced    = True }) hos = OrderPlaced (reconcileStatus hos)
    status _ hos = (completeStatus hos) (updateStatus hos)
    reconcileStatus = justWhen Reconciled      . all isHouseholdOrderReconciled
    updateStatus    = justWhen AwaitingConfirm . any isHouseholdOrderAwaitingConfirm
    completeStatus hos = if all isHouseholdOrderComplete hos 
                           then OrderComplete
                           else OrderOpen

orderTotal :: Order -> Money
orderTotal = sum . map householdOrderTotal . _orderHouseholdOrders

orderAdjustment :: Order -> Maybe OrderAdjustment
orderAdjustment o = 
    if orderTotal o == adjTotal
      then Nothing
      else Just $ OrderAdjustment adjTotal
  where
    adjTotal = sum . map adjHouseholdOrderTotal . _orderHouseholdOrders $ o
    adjHouseholdOrderTotal ho = fromMaybe (householdOrderTotal ho) $ fmap _orderAdjNewTotal $ householdOrderAdjustment ho

orderItems :: Order -> [OrderItem]
orderItems = map (sconcat . NE.fromList) 
           . groupBy ((==) `on` (_productId . _productInfo . _itemProduct))
           . concatMap _householdOrderItems
           . _orderHouseholdOrders

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
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatusFlags = HouseholdOrderStatusFlags
  { _householdOrderIsAbandoned :: Bool
  , _householdOrderIsPlaced :: Bool
  , _householdOrderIsComplete :: Bool
  , _householdOrderUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderAbandoned (Maybe ProductCatalogueUpdateStatus)
                          | HouseholdOrderPlaced    (Maybe OrderReconcileStatus)
                          | HouseholdOrderComplete  (Maybe ProductCatalogueUpdateStatus)
                          | HouseholdOrderOpen      (Maybe ProductCatalogueUpdateStatus)
                            deriving (Eq, Show, Generic)

householdOrder :: OrderInfo -> HouseholdInfo -> HouseholdOrderStatusFlags -> [OrderItem] -> HouseholdOrder
householdOrder orderInfo householdInfo statusFlags items = 
    HouseholdOrder orderInfo
                   householdInfo
                   (status statusFlags items)
                   items
  where
    status (HouseholdOrderStatusFlags { _householdOrderIsAbandoned = True
                                      , _householdOrderUpdated = updated  }) is = HouseholdOrderAbandoned $ updateStatus updated is
    status (HouseholdOrderStatusFlags { _householdOrderIsPlaced    = True }) is = HouseholdOrderPlaced    $ reconcileStatus is
    status (HouseholdOrderStatusFlags { _householdOrderIsComplete  = True
                                      , _householdOrderUpdated = updated  }) is = HouseholdOrderComplete  $ updateStatus updated is
    status (HouseholdOrderStatusFlags { _householdOrderUpdated = updated  }) is = HouseholdOrderOpen      $ updateStatus updated is 
    reconcileStatus      = justWhen Reconciled      . all (isJust . _itemAdjustment)
    updateStatus updated = justWhen AwaitingConfirm . any ((> updated) . _productUpdated . _productInfo . _itemProduct)

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . map itemTotal . _householdOrderItems

householdOrderAdjustment :: HouseholdOrder -> Maybe OrderAdjustment
householdOrderAdjustment ho =
    if householdOrderTotal ho == adjTotal 
      then Nothing 
      else Just $ OrderAdjustment adjTotal
  where
    adjTotal = sum . map adjItemTotal . _householdOrderItems $ ho
    adjItemTotal i = fromMaybe (itemTotal i) $ fmap _itemAdjNewTotal $ _itemAdjustment i

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
isHouseholdOrderAbandoned (HouseholdOrder { _householdOrderStatus = HouseholdOrderAbandoned _ }) = True
isHouseholdOrderAbandoned _ = False

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdOrder -> HouseholdOrder
abandonHouseholdOrder o@(HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete updateStatus }) = o{ _householdOrderStatus = HouseholdOrderAbandoned updateStatus }
abandonHouseholdOrder o@(HouseholdOrder { _householdOrderStatus = HouseholdOrderOpen     updateStatus }) = o{ _householdOrderStatus = HouseholdOrderAbandoned updateStatus }
abandonHouseholdOrder o = o{ _householdOrderStatus = HouseholdOrderAbandoned Nothing }

completeHouseholdOrder :: HouseholdOrder -> HouseholdOrder
completeHouseholdOrder o@(HouseholdOrder { _householdOrderStatus = HouseholdOrderOpen  updateStatus }) = o{ _householdOrderStatus = HouseholdOrderComplete updateStatus }
completeHouseholdOrder o = o{ _householdOrderStatus = HouseholdOrderComplete Nothing }

reopenHouseholdOrder :: HouseholdOrder -> HouseholdOrder
reopenHouseholdOrder o@(HouseholdOrder { _householdOrderStatus = HouseholdOrderComplete  updateStatus }) = o{ _householdOrderStatus = HouseholdOrderOpen updateStatus }
reopenHouseholdOrder o@(HouseholdOrder { _householdOrderStatus = HouseholdOrderAbandoned updateStatus }) = o{ _householdOrderStatus = HouseholdOrderOpen updateStatus }
reopenHouseholdOrder o = o{ _householdOrderStatus = HouseholdOrderOpen Nothing }

updateHouseholdOrderItem :: Product -> (Maybe Int) -> HouseholdOrder -> HouseholdOrder
updateHouseholdOrderItem product maybeQuantity o = o{ _householdOrderItems = items' }
  where
    items = _householdOrderItems o
    items' = addOrUpdate ((== productCode) . itemProductCode) 
                         (orderItem product 1 Nothing) 
                         (updateOrderItemQuantity maybeQuantity) 
                         items
    productCode = _productCode . _productInfo $ product
    itemProductCode = _productCode . _productInfo . _itemProduct

removeHouseholdOrderItem :: ProductId -> HouseholdOrder -> HouseholdOrder
removeHouseholdOrderItem productId o = o{ _householdOrderItems = items' }
  where
    items = _householdOrderItems o
    items' = filter ((/= productId) . _productId . _productInfo . _itemProduct) items

addOrUpdate :: (a -> Bool) -> a -> (a -> a) -> [a] -> [a]
addOrUpdate cond n update (x:xs)
  | cond x = (update x):xs
  | otherwise = x:(addOrUpdate cond n update xs)
addOrUpdate _ n _ [] = [n]

{- OrderItem -}

data OrderItem = OrderItem  
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

orderItem :: Product -> Int -> Maybe OrderItemAdjustment -> OrderItem
orderItem product quantity adjustment = OrderItem product quantity adjustment

itemTotal :: OrderItem -> Money
itemTotal item = price * fromIntegral quantity
  where
    price = _priceAmount . _productPrice . _productInfo . _itemProduct $ item
    quantity = _itemQuantity item

updateOrderItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateOrderItemQuantity quantity i = i{ _itemQuantity = fromMaybe (_itemQuantity i) quantity }

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem (_itemProduct    i1)
                       (_itemQuantity   i1 +  _itemQuantity   i2)
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

{- ProductCatalogueEntry -}

data ProductCatalogueEntry = ProductCatalogueEntry
  { _catalogueEntryCode :: String
  , _catalogueEntryCategory :: String
  , _catalogueEntryBrand :: String
  , _catalogueEntryDescription :: String
  , _catalogueEntryText :: String
  , _catalogueEntrySize :: String
  , _catalogueEntryPrice :: Int
  , _catalogueEntryVatRateType :: VatRateType
  , _catalogueEntryRrp :: Maybe Int
  , _catalogueEntryBiodynamic :: Bool
  , _catalogueEntryFairTrade :: Bool
  , _catalogueEntryGlutenFree :: Bool
  , _catalogueEntryOrganic :: Bool
  , _catalogueEntryAddedSugar :: Bool
  , _catalogueEntryVegan :: Bool
  , _catalogueEntryUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

type ProductCatalogue = [ProductCatalogueEntry]

parseCatalogue :: UTCTime -> String -> ProductCatalogue
parseCatalogue date file =
  catMaybes $ zipWith parse [0..] $ map (splitOn ',') $ drop 1 $ lines file where
  parse i [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
    let price' = fromMaybe 0 $ round . (* 100) <$> (readMaybe price :: Maybe Float)
        vat' = case vat of
                 "1" -> Standard
                 "5" -> Reduced
                 _ -> Zero
        rrp' =  round . (* 100) <$> (readMaybe price :: Maybe Float)
        b' = b == "B"
        f' = f == "F"
        g' = g == "G"
        o' = o == "O"
        s' = s == "S"
        v' = v == "V"
    in  Just $ ProductCatalogueEntry code cat brand desc text size price' vat' rrp' b' f' g' o' s' v' date
  parse _ _ = Nothing

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)

applyCatalogueUpdate :: [Product] -> Order -> Order
applyCatalogueUpdate products order = order

zeroRate :: VatRate
zeroRate = VatRate Zero 1

justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing