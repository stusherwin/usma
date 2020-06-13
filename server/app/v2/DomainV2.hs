{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainV2 where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.HashMap.Lazy as H (HashMap, fromList, lookup)
import           Data.Hashable (Hashable)
import           Data.Time.Clock (UTCTime)
import           Data.Semigroup (Semigroup(..))
import           Data.List (groupBy, maximumBy, find, delete, lookup)
import           Data.Maybe (isJust, maybe, fromMaybe, catMaybes)
import           Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE (fromList)
import           GHC.Generics
import           Prelude hiding (product)
import           Text.Read (readMaybe)

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

householdTotalOrders :: Household -> Int
householdTotalOrders = _moneyIncVat
                     . (sum . map (\ho -> fromMaybe (householdOrderTotal ho) (fmap _orderAdjNewTotal . householdOrderAdjustment $ ho)))
                     .  filter (not . householdOrderIsAbandoned) 
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

{-- OrderGroup --}

newtype OrderGroupId = OrderGroupId 
  { fromOrderGroupId :: Int 
  } deriving (Eq, Show, Generic)

{-- Order --}

data OrderSpec = OrderSpec 
  { _orderSpecCreated :: UTCTime
  , _orderSpecCreatedByHouseholdId :: Maybe HouseholdId
  }

data Order = Order 
  { _orderInfo :: OrderInfo
  , _orderStatusFlags :: OrderStatusFlags
  , _orderHouseholdOrders :: [HouseholdOrder]
  } deriving (Eq, Show, Generic)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Show, Generic)

data OrderInfo = OrderInfo
  { _orderId :: OrderId
  , _orderGroupId :: OrderGroupId
  , _orderCreated :: UTCTime
  , _orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data OrderStatusFlags = OrderStatusFlags
  { _orderIsAbandoned :: Bool
  , _orderIsPlaced :: Bool
  } deriving (Eq, Show, Generic)

data OrderStatus = OrderOpen
                 | OrderAbandoned
                 | OrderComplete
                 | OrderPlaced
                 | OrderReconciled
                   deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Money
  } deriving (Eq, Show, Generic)

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

orderStatus :: Order -> OrderStatus
orderStatus o | orderIsAbandoned o = OrderAbandoned
              | orderIsPlaced o = if orderIsReconciled o 
                                    then OrderReconciled 
                                    else OrderPlaced
              | orderIsComplete o = OrderComplete
              | otherwise = OrderOpen

orderIsAbandoned :: Order -> Bool
orderIsAbandoned = _orderIsAbandoned . _orderStatusFlags

orderIsPlaced :: Order -> Bool
orderIsPlaced = _orderIsPlaced . _orderStatusFlags

orderIsComplete :: Order -> Bool
orderIsComplete = all householdOrderIsComplete . _orderHouseholdOrders

orderIsReconciled :: Order -> Bool
orderIsReconciled = all householdOrderIsReconciled . _orderHouseholdOrders

orderIsAwaitingCatalogueUpdateConfirm :: Order -> Bool
orderIsAwaitingCatalogueUpdateConfirm = any householdOrderIsAwaitingCatalogueUpdateConfirm . _orderHouseholdOrders

overOrderItems :: (OrderItem -> OrderItem) -> Order -> Order
overOrderItems fn o = o{ _orderHouseholdOrders = householdOrders' }
  where
    householdOrders' = map (overHouseholdOrderItems fn) . _orderHouseholdOrders $ o

abandonOrder :: Order -> Order
abandonOrder o = o{ _orderStatusFlags = OrderStatusFlags { _orderIsAbandoned = True, _orderIsPlaced = False }
                  , _orderHouseholdOrders = map abandonHouseholdOrder $ _orderHouseholdOrders o
                  }

placeOrder :: Order -> Order
placeOrder o = o{ _orderStatusFlags = OrderStatusFlags { _orderIsAbandoned = False, _orderIsPlaced = True }
                }

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatusFlags :: HouseholdOrderStatusFlags
  , _householdOrderItems :: [OrderItem]
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatusFlags = HouseholdOrderStatusFlags
  { _householdOrderIsAbandoned :: Bool
  , _householdOrderIsPlaced :: Bool
  , _householdOrderIsComplete :: Bool
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderOpen
                          | HouseholdOrderAbandoned
                          | HouseholdOrderPlaced
                          | HouseholdOrderReconciled
                          | HouseholdOrderComplete
                            deriving (Eq, Show, Generic)

isPastStatus :: HouseholdOrderStatus -> Bool
isPastStatus HouseholdOrderPlaced = True
isPastStatus HouseholdOrderAbandoned = True
isPastStatus HouseholdOrderReconciled = True
--TODO: set compile options to fail on missing case
isPastStatus _ = False

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . map itemTotal . _householdOrderItems

householdOrderAdjustment :: HouseholdOrder -> Maybe OrderAdjustment
householdOrderAdjustment ho =
    if householdOrderTotal ho == adjTotal 
      then Nothing 
      else Just $ OrderAdjustment adjTotal
  where
    adjTotal = sum . map adjItemTotal . _householdOrderItems $ ho
    adjItemTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

householdOrderStatus :: HouseholdOrder -> HouseholdOrderStatus
householdOrderStatus ho | householdOrderIsAbandoned ho = HouseholdOrderAbandoned
                        | householdOrderIsReconciled ho = HouseholdOrderReconciled
                        | householdOrderIsPlaced ho = HouseholdOrderPlaced
                        | householdOrderIsComplete ho = HouseholdOrderComplete
                        | otherwise = HouseholdOrderOpen

householdOrderIsAbandoned :: HouseholdOrder -> Bool
householdOrderIsAbandoned = _householdOrderIsAbandoned . _householdOrderStatusFlags

householdOrderIsComplete :: HouseholdOrder -> Bool
householdOrderIsComplete = _householdOrderIsComplete . _householdOrderStatusFlags

householdOrderIsPlaced :: HouseholdOrder -> Bool
householdOrderIsPlaced = _householdOrderIsPlaced . _householdOrderStatusFlags

householdOrderIsReconciled :: HouseholdOrder -> Bool
householdOrderIsReconciled ho = householdOrderIsPlaced ho && (all (isJust . _itemAdjustment) . _householdOrderItems $ ho)

householdOrderIsAwaitingCatalogueUpdateConfirm :: HouseholdOrder -> Bool
householdOrderIsAwaitingCatalogueUpdateConfirm ho = 
     not (householdOrderIsAbandoned ho)
  && not (householdOrderIsPlaced ho)
  && any (isJust . _itemAdjustment) (_householdOrderItems ho)

overHouseholdOrderItems :: (OrderItem -> OrderItem) -> HouseholdOrder -> HouseholdOrder
overHouseholdOrderItems fn ho = ho{ _householdOrderItems = items' }
  where
    items' = map fn . _householdOrderItems $ ho

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdOrder -> HouseholdOrder
abandonHouseholdOrder ho@(HouseholdOrder { _householdOrderStatusFlags = f }) =
    ho{ _householdOrderStatusFlags = f{ _householdOrderIsAbandoned = True
                                      , _householdOrderIsPlaced = False 
                                      , _householdOrderIsComplete = False
                                      } }

completeHouseholdOrder :: HouseholdOrder -> HouseholdOrder
completeHouseholdOrder ho@(HouseholdOrder { _householdOrderStatusFlags = f }) =
  ho{ _householdOrderStatusFlags = f{ _householdOrderIsAbandoned = False
                                    , _householdOrderIsPlaced = False 
                                    , _householdOrderIsComplete = True
                                    } }

reopenHouseholdOrder :: HouseholdOrder -> HouseholdOrder
reopenHouseholdOrder ho@(HouseholdOrder { _householdOrderStatusFlags = f }) =
  ho{ _householdOrderStatusFlags = f{ _householdOrderIsAbandoned = False
                                    , _householdOrderIsPlaced = False 
                                    , _householdOrderIsComplete = False
                                    } }

updateHouseholdOrderItem :: ProductCatalogueEntry -> (Maybe Int) -> HouseholdOrder -> HouseholdOrder
updateHouseholdOrderItem entry maybeQuantity o = o{ _householdOrderItems = items' }
  where
    items = _householdOrderItems o
    items' = addOrUpdate ((== entryCode) . itemProductCode) 
                         (OrderItem (fromCatalogueEntry entry) 1 Nothing) 
                         (updateOrderItemQuantity maybeQuantity) 
                         items
    entryCode = _catalogueEntryCode entry

removeHouseholdOrderItem :: ProductCode -> HouseholdOrder -> HouseholdOrder
removeHouseholdOrderItem productCode o = o{ _householdOrderItems = items' }
  where
    items = _householdOrderItems o
    items' = filter ((/= productCode) . itemProductCode) items

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

itemProductId :: OrderItem -> ProductId
itemProductId = _productId . _productInfo . _itemProduct

itemProductCode :: OrderItem -> ProductCode
itemProductCode = _productCode . _productInfo . _itemProduct

itemProductPrice :: OrderItem -> Price
itemProductPrice = _productPrice . _productInfo . _itemProduct

itemTotal :: OrderItem -> Money
itemTotal item = price * quantity
  where
    price = _priceAmount . itemProductPrice $ item
    quantity = fromIntegral $ _itemQuantity item

updateOrderItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateOrderItemQuantity quantity i = i{ _itemQuantity = fromMaybe (_itemQuantity i) quantity }

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem (_itemProduct    i1)
                       (_itemQuantity   i1 +  _itemQuantity   i2)
                       (_itemAdjustment i1 <> _itemAdjustment i2)

data OrderItemAdjustment = OrderItemAdjustment 
  { _itemAdjNewPrice :: Price
  , _itemAdjNewQuantity :: Int
  , _itemAdjIsDiscontinued :: Bool
  , _itemAdjDate :: UTCTime
  } deriving (Eq, Show, Generic)

instance Semigroup OrderItemAdjustment where
  a1 <> a2 = OrderItemAdjustment latestPrice
                                 totalQuantity
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

itemAdjNewTotal :: OrderItemAdjustment -> Money
itemAdjNewTotal adj = atQuantity (_itemAdjNewQuantity adj) (_itemAdjNewPrice adj)

{- Product -}

newtype ProductId = ProductId
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

newtype ProductCode = ProductCode
  { fromProductCode :: String 
  } deriving (Eq, Show, Generic)

instance Hashable ProductCode

data ProductInfo = ProductInfo
  { _productId :: ProductId
  , _productCode :: ProductCode
  , _productName :: String
  , _productPrice :: Price
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

productId :: Product -> ProductId
productId = _productId . _productInfo

productCode :: Product -> ProductCode
productCode = _productCode . _productInfo

productPrice :: Product -> Price
productPrice = _productPrice . _productInfo

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

getVatRate :: VatRateType -> [VatRate] -> VatRate
getVatRate t vs =fromMaybe zeroRate $ lookup t $ map (_vatRateType &&& id) $ vs

{- ProductCatalogue -}

data ProductCatalogueEntry = ProductCatalogueEntry
  { _catalogueEntryCode :: ProductCode
  , _catalogueEntryCategory :: String
  , _catalogueEntryBrand :: String
  , _catalogueEntryDescription :: String
  , _catalogueEntryText :: String
  , _catalogueEntrySize :: String
  , _catalogueEntryPrice :: Price
  , _catalogueEntryRrp :: Maybe Int
  , _catalogueEntryBiodynamic :: Bool
  , _catalogueEntryFairTrade :: Bool
  , _catalogueEntryGlutenFree :: Bool
  , _catalogueEntryOrganic :: Bool
  , _catalogueEntryAddedSugar :: Bool
  , _catalogueEntryVegan :: Bool
  , _catalogueEntryUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

type ProductCatalogue = H.HashMap ProductCode ProductCatalogueEntry

parseCatalogue :: [VatRate] -> UTCTime -> String -> ProductCatalogue
parseCatalogue vatRates date file =
    H.fromList 
    . map (_catalogueEntryCode &&& id) 
    . catMaybes 
    . zipWith parse [0..] 
    . map (splitOn ',')
    . drop 1 
    . lines 
    $ file
  where
    parse i [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
      let code' = ProductCode code
          vatRateType = case vat of
            "1" -> Standard
            "5" -> Reduced
            _ -> Zero
          vatRate = getVatRate vatRateType vatRates     
          price' = atVatRate vatRate $ fromMaybe 0 $ round . (* 100) <$> (readMaybe price :: Maybe Float)
          rrp' =  round . (* 100) <$> (readMaybe price :: Maybe Float)
          b' = b == "B"
          f' = f == "F"
          g' = g == "G"
          o' = o == "O"
          s' = s == "S"
          v' = v == "V"
      in  Just $ ProductCatalogueEntry code' cat brand desc text size price' rrp' b' f' g' o' s' v' date
    parse _ _ = Nothing

applyCatalogueUpdate :: UTCTime -> ProductCatalogue -> HouseholdOrder -> HouseholdOrder
applyCatalogueUpdate date products = overHouseholdOrderItems apply
  where
    apply item = case H.lookup (itemProductCode item) products of
                   Just e -> adjust e    (_itemAdjustment item) item
                   _      -> discontinue (_itemAdjustment item) item

    adjust e (Just a) i | itemProductPrice i /= _catalogueEntryPrice e = 
                            i{ _itemAdjustment = Just a{ _itemAdjNewPrice = _catalogueEntryPrice e
                                                       , _itemAdjDate = date 
                                                       }
                             }
                        | otherwise = i
    adjust e _ i  = i{ _itemAdjustment = Just $ OrderItemAdjustment (_catalogueEntryPrice e) 
                                                                    (_itemQuantity i) 
                                                                    False 
                                                                    date 
                     }

    discontinue (Just a) i = i{ _itemAdjustment = Just a{ _itemAdjNewQuantity = 0
                                                        , _itemAdjIsDiscontinued = True
                                                        , _itemAdjDate = date 
                                                        }
                              }
    discontinue _  i = i{ _itemAdjustment = Just $ OrderItemAdjustment (itemProductPrice i) 0 True date }

acceptCatalogueUpdates :: UTCTime -> HouseholdOrder -> HouseholdOrder
acceptCatalogueUpdates date = id
-- acceptCatalogueUpdates date order = let o = overHouseholdOrderItems accept
--                                         f = _householdOrderStatusFlags o
--                                     in  o{ _householdOrderStatusFlags = f{ _householdOrderUpdated = date } }
--   where
--     accept i@(OrderItem { _itemAdjustment = Just (OrderItemAdjustment { { _itemAdjNewPrice :: Price
--                                                                         , _itemAdjNewQuantity :: Int
--                                                                         , _itemAdjIsDiscontinued :: Bool
--                                                                         , _itemAdjDate :: UTCTime
--                                                                         }})) =
--       i{ _itemAdjustment = Nothing }

fromCatalogueEntry :: ProductCatalogueEntry -> Product
fromCatalogueEntry entry = undefined

{- Utils -}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
                            
justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing