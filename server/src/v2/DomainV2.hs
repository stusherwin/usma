{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module DomainV2 where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.HashMap.Lazy as H (HashMap, fromList, lookup, elems)
import           Data.Hashable (Hashable)
import           Data.Time.Clock (UTCTime)
import           Data.List (maximumBy, find, partition, sortBy)
import           Data.List.Extra (trim, lower)
import           Data.Maybe (isJust, fromMaybe, catMaybes)
import           Data.Ord (comparing)
import           Data.Semigroup (sconcat)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE (groupBy, nonEmpty, toList)
import           GHC.Generics
import           Prelude hiding (product)
import           Text.Read (readMaybe)

import Debug.Trace(trace)

{- Household -}

data Household = Household 
  { _householdInfo :: HouseholdInfo
  , _householdContact :: Contact
  , _householdOrders :: [HouseholdOrder]
  , _householdPayments :: [Payment]
  } deriving (Eq, Show, Generic)

data Contact = Contact
  { _contactName :: Maybe String
  , _contactEmail :: Maybe String
  , _contactPhone :: Maybe String
  } deriving (Eq, Show, Generic)

data HouseholdSpec = HouseholdSpec
  { _householdSpecName :: String
  , _householdSpecContact :: Contact
  } deriving (Eq, Show, Generic)

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Ord, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

householdTotalOrders :: Household -> Int
householdTotalOrders = _moneyIncVat
                     . (sum . map householdOrderAdjTotal)
                     . filter ((/= HouseholdOrderAbandoned) . _householdOrderStatus .&&. (/= OrderAbandoned) . _householdOrderOrderStatus)
                     . _householdOrders

householdTotalPayments :: Household -> Int
householdTotalPayments = (sum . map _paymentAmount)
                       . _householdPayments 

householdBalance :: Household -> Int
householdBalance h = householdTotalPayments h - householdTotalOrders h

updateHousehold :: String -> Contact -> Household -> Household
updateHousehold name contact h = let i = _householdInfo h
                                 in  h{ _householdInfo = i{ _householdName = name }
                                      , _householdContact = contact 
                                      }

{- Payment -}

data Payment = Payment 
  { _paymentId :: PaymentId
  , _paymentHouseholdId :: HouseholdId
  , _paymentDate :: UTCTime
  , _paymentAmount :: Int
  } deriving (Eq, Show, Generic)

data PaymentSpec = PaymentSpec
  { _paymentSpecHouseholdId :: HouseholdId
  , _paymentSpecDate :: UTCTime
  , _paymentSpecAmount :: Int
  } deriving (Eq, Show, Generic)

newtype PaymentId = PaymentId 
  { fromPaymentId :: Int 
  } deriving (Eq, Ord, Show, Generic)

updatePayment :: UTCTime -> Int -> Payment -> Payment
updatePayment date amount p = p{ _paymentDate = date
                               , _paymentAmount = amount 
                               }

{-- OrderGroup --}

data OrderGroup = OrderGroup
  { _groupId :: OrderGroupId
  , _groupName :: String
  , _groupKey :: String
  , _groupSettings :: OrderGroupSettings
  }

data OrderGroupSettings = OrderGroupSettings
  { _groupSettingsPaymentsEnabled :: Bool
  }

newtype OrderGroupId = OrderGroupId 
  { fromOrderGroupId :: Int 
  } deriving (Eq, Ord, Show, Generic)

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
  } deriving (Eq, Ord, Show, Generic)

data OrderInfo = OrderInfo
  { _orderId :: OrderId
  , _orderGroupId :: OrderGroupId
  , _orderCreated :: UTCTime
  , _orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data OrderStatus = OrderOpen
                 | OrderAbandoned
                 | OrderPlaced
                   deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Money
  } deriving (Eq, Show, Generic)

orderId :: Order -> OrderId
orderId = _orderId . _orderInfo

orderTotal :: Order -> Money
orderTotal = sum . map itemTotal . orderItems

orderAdjustment :: Order -> Maybe OrderAdjustment
orderAdjustment o = 
    if any (isJust . householdOrderAdjustment) $ householdOrders o
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . map householdOrderAdjTotal . householdOrders $ o

orderItems :: Order -> [OrderItem]
orderItems = map sconcat
           . NE.groupBy ((==) `on` itemProductCode)
           . sortBy (compare `on` itemProductCode)
           . concatMap _householdOrderItems
           . householdOrders

householdOrders :: Order -> [HouseholdOrder]
householdOrders = filter ((/= HouseholdOrderAbandoned) . _householdOrderStatus) . _orderHouseholdOrders

orderIsComplete :: Order -> Bool
orderIsComplete = (/= OrderAbandoned) . _orderStatus 
             .&&. (not . null) . householdOrders
             .&&. all ((== HouseholdOrderComplete) . _householdOrderStatus) . householdOrders

orderIsReconciled :: Order -> Bool
orderIsReconciled = (== OrderPlaced) . _orderStatus 
               .&&. all householdOrderIsReconciled . householdOrders

orderIsAwaitingCatalogueUpdateConfirm :: Order -> Bool
orderIsAwaitingCatalogueUpdateConfirm = any householdOrderIsAwaitingCatalogueUpdateConfirm . householdOrders

overHouseholdOrders :: ([HouseholdOrder] -> [HouseholdOrder]) -> Order -> Order
overHouseholdOrders fn o = o{ _orderHouseholdOrders = fn $ _orderHouseholdOrders $ o }

overHouseholdOrders' :: (Order -> [HouseholdOrder] -> [HouseholdOrder]) -> Order -> Order
overHouseholdOrders' fn o = o{ _orderHouseholdOrders = fn o $ _orderHouseholdOrders $ o }

abandonOrder :: Order -> Order
abandonOrder o = overHouseholdOrders (map $ abandonIfNotComplete . updateOrderStatus OrderAbandoned) o{ _orderStatus = OrderAbandoned }
                 
placeOrder :: Order -> Order
placeOrder o = overHouseholdOrders (map $ updateOrderStatus OrderPlaced) o{ _orderStatus = OrderPlaced }

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdId -> Order -> Order
abandonHouseholdOrder householdId = 
    (overHouseholdOrders' $ \o -> map $ updateOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . (overHouseholdOrders $ update (whereHouseholdId householdId) abandon)

completeHouseholdOrder :: HouseholdId -> Order -> Order
completeHouseholdOrder householdId = 
    overHouseholdOrders $ update (whereHouseholdId householdId) complete

reopenHouseholdOrder :: ProductCatalogue -> HouseholdId -> Order -> Order
reopenHouseholdOrder catalogue householdId = 
    (overHouseholdOrders' $ \o -> map $ updateOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . (overHouseholdOrders $ update (whereHouseholdId householdId) $ applyUpdate catalogue . reopen)

updateOrderAbandonedStatus :: Order -> Order
updateOrderAbandonedStatus o = o{ _orderStatus = if all ((== HouseholdOrderAbandoned) . _householdOrderStatus) $ _orderHouseholdOrders o
                                                   then OrderAbandoned
                                                   else OrderOpen
                                }

addOrUpdateHouseholdOrderItems :: ProductCatalogue -> HouseholdInfo  -> [(ProductCode, Maybe Int)] -> Order -> Order
addOrUpdateHouseholdOrderItems catalogue household itemQuantities =
    (overHouseholdOrders $ update (whereHousehold household) $ addOrUpdateItems catalogue itemQuantities)
  . ensureHouseholdOrder household

addItemsFromPastOrder :: ProductCatalogue -> HouseholdInfo -> Order -> Order -> Order
addItemsFromPastOrder catalogue household pastOrder = 
    (overHouseholdOrders $ update (whereHousehold household) $ addOrUpdateItems catalogue pastItemQuantities)
  . ensureHouseholdOrder household
  where
    pastItemQuantities = map (itemProductCode &&& Just . (const 1))-- TODO: Should be . _itemQuantity)
                       . concatMap _householdOrderItems
                       . filter (whereHousehold household)
                       . _orderHouseholdOrders 
                       $ pastOrder

ensureHouseholdOrder :: HouseholdInfo -> Order -> Order
ensureHouseholdOrder household o = 
    (overHouseholdOrders $ ensure (whereHousehold household) $ newHouseholdOrder o household) o

removeHouseholdOrderItem :: HouseholdId -> ProductCode -> Order -> Order
removeHouseholdOrderItem householdId productCode = 
    overHouseholdOrders 
  $ update (whereHouseholdId householdId)
  $ removeItem productCode

applyCatalogueUpdate :: ProductCatalogue -> Order -> Order
applyCatalogueUpdate catalogue = 
    overHouseholdOrders 
  $ update ((/= HouseholdOrderAbandoned) . _householdOrderStatus)
  $ applyUpdate catalogue

acceptCatalogueUpdate :: HouseholdId -> Order -> Order
acceptCatalogueUpdate householdId =
    overHouseholdOrders
  $ update (whereHouseholdId householdId)
  $ acceptUpdate

reconcileOrderItems :: UTCTime -> [(HouseholdId, OrderItemSpec)] -> Order -> Order
reconcileOrderItems date specs = 
    overHouseholdOrders (map reconcile)
  where
    reconcile ho = let updatesForHousehold = map snd . filter (((==) (householdOrderHouseholdId ho)) . fst) $ specs
                   in  reconcileItems date updatesForHousehold ho

whereHouseholdId :: HouseholdId -> HouseholdOrder -> Bool
whereHouseholdId householdId = (== householdId) . householdOrderHouseholdId

whereHousehold :: HouseholdInfo -> HouseholdOrder -> Bool
whereHousehold household = (== _householdId household) . householdOrderHouseholdId

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderOrderStatus :: OrderStatus
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatus :: HouseholdOrderStatus
  , _householdOrderItems :: [OrderItem]
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderOpen
                          | HouseholdOrderComplete
                          | HouseholdOrderAbandoned 
                          deriving (Eq, Show, Generic)

newHouseholdOrder :: Order -> HouseholdInfo -> HouseholdOrder
newHouseholdOrder order household = HouseholdOrder (_orderInfo order) (_orderStatus order) household HouseholdOrderOpen []

householdOrderHouseholdId :: HouseholdOrder -> HouseholdId
householdOrderHouseholdId = _householdId . _householdOrderHouseholdInfo

householdOrderHouseholdName :: HouseholdOrder -> String
householdOrderHouseholdName = _householdName . _householdOrderHouseholdInfo

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . map itemTotal . _householdOrderItems

householdOrderAdjTotal :: HouseholdOrder -> Money
householdOrderAdjTotal ho = fromMaybe (householdOrderTotal ho) $ fmap _orderAdjNewTotal . householdOrderAdjustment $ ho

householdOrderAdjustment :: HouseholdOrder -> Maybe OrderAdjustment
householdOrderAdjustment ho =
    if any (isJust . _itemAdjustment) (_householdOrderItems ho)
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . map adjItemTotal . _householdOrderItems $ ho
    adjItemTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

householdOrderIsReconciled :: HouseholdOrder -> Bool
householdOrderIsReconciled = all (isJust . _itemAdjustment) . _householdOrderItems

householdOrderIsAwaitingCatalogueUpdateConfirm :: HouseholdOrder -> Bool
householdOrderIsAwaitingCatalogueUpdateConfirm = any (isJust . _itemAdjustment) .  _householdOrderItems

overHouseholdOrderItems :: ([OrderItem] -> [OrderItem]) -> HouseholdOrder -> HouseholdOrder
overHouseholdOrderItems fn ho = ho{ _householdOrderItems = fn $ _householdOrderItems $ ho }

updateOrderStatus :: OrderStatus -> HouseholdOrder -> HouseholdOrder
updateOrderStatus status ho = ho { _householdOrderOrderStatus = status }

abandon :: HouseholdOrder -> HouseholdOrder
abandon ho = ho { _householdOrderStatus = HouseholdOrderAbandoned }

complete :: HouseholdOrder -> HouseholdOrder
complete ho = ho { _householdOrderStatus = HouseholdOrderComplete }

reopen :: HouseholdOrder -> HouseholdOrder
reopen ho = ho { _householdOrderStatus = HouseholdOrderOpen }

abandonIfNotComplete :: HouseholdOrder -> HouseholdOrder
abandonIfNotComplete ho | (== HouseholdOrderComplete) . _householdOrderStatus $ ho = ho 
                        | otherwise = abandon ho

addOrUpdateItems :: ProductCatalogue -> [(ProductCode, Maybe Int)] -> HouseholdOrder -> HouseholdOrder
addOrUpdateItems catalogue = overHouseholdOrderItems
                           . addOrUpdate ((==) `on` itemProductCode) (updateItemQuantity . Just . _itemQuantity)
                           . catMaybes 
                           . map toOrderItem
  where
    toOrderItem (code, quantity) = findEntry code catalogue <&> \e ->
      OrderItem (fromCatalogueEntry e) (fromMaybe 1 quantity) Nothing

removeItem :: ProductCode -> HouseholdOrder -> HouseholdOrder
removeItem productCode = overHouseholdOrderItems $ filter ((/= productCode) . itemProductCode)

applyUpdate :: ProductCatalogue -> HouseholdOrder -> HouseholdOrder
applyUpdate catalogue = overHouseholdOrderItems $ map apply
  where
    apply item = case findEntry (itemProductCode item) catalogue of
      Just e | itemProductPrice item == _catalogueEntryPrice e -> item
             | otherwise -> adjustItemPrice date (_catalogueEntryPrice e) item
      _ -> discontinueProduct date item
    date = getDate catalogue

acceptUpdate :: HouseholdOrder -> HouseholdOrder
acceptUpdate = overHouseholdOrderItems $ map removeItemAdjustment
                                       . removeDiscontinued
                                       . map accept
  where
    removeDiscontinued = filter (not . fromMaybe False . fmap _itemAdjIsDiscontinued . _itemAdjustment)
    accept item@OrderItem 
        { _itemAdjustment = Just (OrderItemAdjustment 
          { _itemAdjNewPrice = price
          , _itemAdjNewQuantity = quantity })
        } = 
      updateItemPrice price . updateItemQuantity (Just quantity) $ item
    accept item = item

reconcileItems :: UTCTime -> [OrderItemSpec] -> HouseholdOrder -> HouseholdOrder
reconcileItems date specs = overHouseholdOrderItems $ map reconcile
  where
    reconcile item = case find ((== itemProductCode item) . _itemSpecProductCode) specs of
      Just spec -> adjustItemPrice date (reprice (_itemSpecProductPrice spec) (itemProductPrice item))
                 $ adjustItemQuantity date (_itemSpecQuantity spec) item
      _ -> item

{- OrderItem -}

data OrderItem = OrderItem  
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

data OrderItemSpec = OrderItemSpec
  { _itemSpecProductCode :: ProductCode
  , _itemSpecProductPrice :: Int
  , _itemSpecQuantity :: Int
  } deriving (Eq, Show, Generic)

itemProductCode :: OrderItem -> ProductCode
itemProductCode = _productCode . _productInfo . _itemProduct

itemProductName :: OrderItem -> String
itemProductName = _productName . _productInfo . _itemProduct

itemProductPrice :: OrderItem -> Price
itemProductPrice = _productPrice . _productInfo . _itemProduct

itemTotal :: OrderItem -> Money
itemTotal item = atQuantity (_itemQuantity item) (itemProductPrice item)

itemAdjTotal :: OrderItem -> Money
itemAdjTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

updateItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateItemQuantity quantity i = i{ _itemQuantity = fromMaybe (_itemQuantity i) quantity }

updateItemPrice :: Price -> OrderItem -> OrderItem
updateItemPrice price i@OrderItem { _itemProduct = p@Product { _productInfo = pi } } = 
  i{ _itemProduct = p{ _productInfo = pi{ _productPrice = price } } }

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem p (q1 + q2) (a1 <> a2)
    where
      p        = product (_itemProduct i1) (_itemAdjustment i1) (_itemProduct i2) (_itemAdjustment i2)
      (a1, a2) = adjustments (_itemAdjustment i1) (_itemAdjustment i2)
      (q1, q2) = (_itemQuantity i1, _itemQuantity i2)

      product _  Nothing p2 (Just _) = p2
      product p1 _       _  _        = p1

      adjustments (Just a1) Nothing   = (Just a1, Just $ withNewQuantity i2 a1)
      adjustments Nothing   (Just a2) = (Just $ withNewQuantity i1 a2, Just a2)
      adjustments a1        a2        = (a1, a2)

      withNewQuantity i a = a { _itemAdjNewQuantity = if _itemAdjIsDiscontinued a then 0 else _itemQuantity i }

data OrderItemAdjustment = OrderItemAdjustment 
  { _itemAdjNewPrice :: Price
  , _itemAdjNewQuantity :: Int
  , _itemAdjIsDiscontinued :: Bool
  , _itemAdjDate :: UTCTime
  } deriving (Eq, Show, Generic)

instance Semigroup OrderItemAdjustment where
  a1 <> a2 = OrderItemAdjustment latestPrice totalQuantity discontinued latestDate
    where 
      discontinued  = _itemAdjIsDiscontinued a1 || _itemAdjIsDiscontinued a2
      totalQuantity = if discontinued
                        then 0 
                        else _itemAdjNewQuantity a1 + _itemAdjNewQuantity a2
      latest        = maximumBy (comparing _itemAdjDate) [a1, a2]
      latestPrice   = _itemAdjNewPrice latest
      latestDate    = _itemAdjDate latest

itemAdjNewTotal :: OrderItemAdjustment -> Money
itemAdjNewTotal adj = atQuantity (_itemAdjNewQuantity adj) (_itemAdjNewPrice adj)

adjustItemPrice :: UTCTime -> Price -> OrderItem -> OrderItem
adjustItemPrice date price i@OrderItem { _itemAdjustment = Just a } = 
  i{ _itemAdjustment = Just a{ _itemAdjNewPrice = price
                             , _itemAdjDate = date 
                             }
   }
adjustItemPrice date price i = 
  i{ _itemAdjustment = Just $ OrderItemAdjustment price
                                                  (_itemQuantity i)
                                                  False
                                                  date
   }

adjustItemQuantity :: UTCTime -> Int -> OrderItem -> OrderItem
adjustItemQuantity date quantity i@OrderItem { _itemAdjustment = Just a } = 
  i{ _itemAdjustment = Just a{ _itemAdjNewQuantity = quantity
                             , _itemAdjDate = date 
                             }
   }
adjustItemQuantity date quantity i = 
  i{ _itemAdjustment = Just $ OrderItemAdjustment (itemProductPrice i)
                                                  quantity
                                                  False
                                                  date
   }

discontinueProduct :: UTCTime -> OrderItem -> OrderItem
discontinueProduct date i@OrderItem { _itemAdjustment = Just a } = 
  i{ _itemAdjustment = Just a{ _itemAdjNewQuantity = 0
                             , _itemAdjIsDiscontinued = True
                             , _itemAdjDate = date 
                             }
   }
discontinueProduct date i = i{ _itemAdjustment = Just $ OrderItemAdjustment (itemProductPrice i) 0 True date }

removeItemAdjustment :: OrderItem -> OrderItem
removeItemAdjustment i@OrderItem { _itemAdjustment = Just _ } = i{ _itemAdjustment = Nothing }
removeItemAdjustment i = i

{- Product -}

newtype ProductId = ProductId
  { fromProductId :: Int 
  } deriving (Eq, Ord, Show, Generic)

newtype ProductCode = ProductCode
  { fromProductCode :: String 
  } deriving (Eq, Ord, Show, Generic)

instance Hashable ProductCode

data ProductInfo = ProductInfo
  { _productCode :: ProductCode
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
atVatRate vatRate amountExcVat = Price vatRate (Money amountExcVat amountIncVat)
  where
    amountIncVat = round $ fromIntegral amountExcVat * (_vatRateMultiplier vatRate)

atQuantity :: Int -> Price -> Money
atQuantity quantity price = _priceAmount $ atVatRate vatRate (amountExcVat * fromIntegral quantity)
  where
    vatRate = _priceVatRate price
    amountExcVat = _moneyExcVat . _priceAmount $ price

reprice :: Int -> Price -> Price
reprice newAmountExcVat (Price vatRate _) = atVatRate vatRate newAmountExcVat

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

standardRate :: VatRate
standardRate = VatRate Standard 1.2

reducedRate :: VatRate
reducedRate = VatRate Reduced 1.05

getVatRate :: VatRateType -> [VatRate] -> VatRate
getVatRate t vs = fromMaybe zeroRate $ lookup t $ map (_vatRateType &&& id) $ vs

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

newtype ProductCatalogue = ProductCatalogue { fromProductCatalogue :: H.HashMap ProductCode ProductCatalogueEntry }

productCatalogue :: [ProductCatalogueEntry] -> ProductCatalogue
productCatalogue = ProductCatalogue . H.fromList . map (_catalogueEntryCode &&& id)

findEntry :: ProductCode -> ProductCatalogue -> Maybe ProductCatalogueEntry
findEntry code = H.lookup code . fromProductCatalogue

getEntries :: ProductCatalogue -> [ProductCatalogueEntry]
getEntries = sortBy (compare `on` _catalogueEntryCode) . H.elems . fromProductCatalogue

--TODO: Safe version? If catalogue empty
getDate :: ProductCatalogue -> UTCTime
getDate = _catalogueEntryUpdated . head . H.elems . fromProductCatalogue

parseCatalogue :: [VatRate] -> UTCTime -> String -> ProductCatalogue
parseCatalogue vatRates date file =
      productCatalogue
    . catMaybes 
    . map parse 
    . map (splitOn ',')
    . drop 1 
    . lines 
    $ file
  where
    parse [cat,brand,code,desc,text,size,price,vat,_,b,f,g,o,s,v,_] = 
      let code' = ProductCode $ trim code
          vatRateType = case trim vat of
            "1" -> Standard
            "5" -> Reduced
            _ -> Zero
          vatRate = getVatRate vatRateType vatRates     
          price' = atVatRate vatRate $ fromMaybe 0 $ round . (* 100) <$> (readMaybe (trim price) :: Maybe Float)
          rrp' =  round . (* 100) <$> (readMaybe (trim price) :: Maybe Float)
          b' = trim b == "B"
          f' = trim f == "F"
          g' = trim g == "G"
          o' = trim o == "O"
          s' = trim s == "S"
          v' = trim v == "V"
      in  Just $ ProductCatalogueEntry code' cat brand desc text size price' rrp' b' f' g' o' s' v' date
    parse _ = Nothing

fromCatalogueEntry :: ProductCatalogueEntry -> Product
fromCatalogueEntry e = Product 
  { _productInfo = ProductInfo
    { _productCode = _catalogueEntryCode e
    , _productName = productName e
    , _productPrice = _catalogueEntryPrice e
    }
  , _productFlags = ProductFlags
    { _productIsBiodynamic = _catalogueEntryBiodynamic e
    , _productIsFairTrade = _catalogueEntryFairTrade e
    , _productIsGlutenFree = _catalogueEntryGlutenFree e
    , _productIsOrganic = _catalogueEntryOrganic e
    , _productIsAddedSugar = _catalogueEntryAddedSugar e
    , _productIsVegan = _catalogueEntryVegan e
    }
  }

productName :: ProductCatalogueEntry -> String
productName e = unwords
              $ filter (not . null)
              [ _catalogueEntryBrand e
              , _catalogueEntryDescription $ e
              , if null (_catalogueEntrySize e)
                  then ""
                  else "(" ++ (lower . _catalogueEntrySize $ e) ++ ")"
              , _catalogueEntryText $ e
              ]
 
{- Utils -}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f _ [] = []
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
                            
justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing

addOrUpdate :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
addOrUpdate _ _ [] ys = ys
addOrUpdate eq update (x : xs) ys = 
  case partition (eq x) ys of
    (y : _, ys') -> (update x y) : addOrUpdate eq update xs ys' 
    _ -> x : addOrUpdate eq update xs ys

ensure :: (a -> Bool) -> a -> [a] -> [a]
ensure = ensure' False
  where
    ensure' False _ new [] = [new]
    ensure' _ _ _ [] = []
    ensure' found eq new (x : xs)
      | eq x      = x : ensure' True  eq new xs
      | otherwise = x : ensure' found eq new xs

update :: (a -> Bool) -> (a -> a) -> [a] -> [a]
update _ _ [] = []
update cond fn (x:xs) | cond x = fn x : update cond fn xs
                      | otherwise = x : update cond fn xs

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .&&. f2 = \x -> f1 x && f2 x
infixr 3 .&&.

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .||. f2 = \x -> f1 x || f2 x
infixr 2 .||.