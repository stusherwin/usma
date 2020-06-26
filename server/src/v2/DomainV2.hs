{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module DomainV2 where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.HashMap.Lazy as H (HashMap, fromList, lookup, elems)
import           Data.Hashable (Hashable)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Calendar (Day)
import           Data.Semigroup (Semigroup(..), sconcat)
import           Data.List (groupBy, maximumBy, find, delete, lookup, partition, sortBy)
import           Data.List.Extra (trim, lower)
import           Data.Maybe (isJust, maybe, fromMaybe, catMaybes)
import           Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE (fromList)
import           GHC.Generics
import           Prelude hiding (product)
import           Text.Read (readMaybe)

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
  } deriving (Eq, Show, Generic)

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

orderId :: Order -> OrderId
orderId = _orderId . _orderInfo

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
           . groupBy ((==) `on` itemProductCode)
           . sortBy (compare `on` fromProductCode . itemProductCode)
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

overHouseholdOrders :: ([HouseholdOrder] -> [HouseholdOrder]) -> Order -> Order
overHouseholdOrders fn o = o{ _orderHouseholdOrders = fn $ _orderHouseholdOrders $ o }

abandonOrder :: Order -> Order
abandonOrder o = let o' = overHouseholdOrders (map abandon) o
                 in  o'{ _orderStatusFlags = OrderStatusFlags { _orderIsAbandoned = True, _orderIsPlaced = False } }
                 
placeOrder :: Order -> Order
placeOrder o = o{ _orderStatusFlags = OrderStatusFlags { _orderIsAbandoned = False, _orderIsPlaced = True } }

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdId -> Order -> Order
abandonHouseholdOrder householdId o = 
    o{ _orderHouseholdOrders = householdOrders' 
     , _orderStatusFlags = OrderStatusFlags 
       { _orderIsAbandoned = all householdOrderIsAbandoned householdOrders'
       , _orderIsPlaced = False
       }
     }
  where 
    householdOrders' = mapWhere ((== householdId) . householdOrderHouseholdId) abandon
                     $ _orderHouseholdOrders o

completeHouseholdOrder :: HouseholdId -> Order -> Order
completeHouseholdOrder householdId = 
    overHouseholdOrders 
  $ mapWhere ((== householdId) . householdOrderHouseholdId) 
  $ complete

reopenHouseholdOrder :: ProductCatalogue -> HouseholdId -> Order -> Order
reopenHouseholdOrder catalogue householdId o = 
    o{ _orderHouseholdOrders = householdOrders' 
     , _orderStatusFlags = OrderStatusFlags 
       { _orderIsAbandoned = all householdOrderIsAbandoned householdOrders'
       , _orderIsPlaced = False
       }
     }
  where 
    householdOrders' = mapWhere ((== householdId) . householdOrderHouseholdId) (reopen . (applyUpdate catalogue)) 
                     $ _orderHouseholdOrders o

addOrUpdateHouseholdOrderItems :: ProductCatalogue -> HouseholdId  -> [(ProductCode, Maybe Int)] -> Order -> Order
addOrUpdateHouseholdOrderItems catalogue householdId itemQuantities =
    overHouseholdOrders 
  $ mapWhere ((== householdId) . householdOrderHouseholdId) 
  $ addOrUpdateItems catalogue itemQuantities

addItemsFromPastOrder :: ProductCatalogue -> HouseholdId -> Order -> Order -> Order
addItemsFromPastOrder catalogue householdId pastOrder = 
  overHouseholdOrders 
  $ mapWhere ((== householdId) . householdOrderHouseholdId) 
  $ addOrUpdateItems catalogue pastItemQuantities
  where
    pastItemQuantities = map (itemProductCode &&& Just . (const 1))-- TODO: Should be . _itemQuantity)
                       . concatMap _householdOrderItems
                       . filter ((== householdId) . householdOrderHouseholdId) 
                       . _orderHouseholdOrders 
                       $ pastOrder


removeHouseholdOrderItem :: HouseholdId -> ProductCode -> Order -> Order
removeHouseholdOrderItem householdId productCode = 
    overHouseholdOrders 
  $ mapWhere ((== householdId) . householdOrderHouseholdId) 
  $ removeItem productCode

applyCatalogueUpdate :: ProductCatalogue -> Order -> Order
applyCatalogueUpdate catalogue = 
    overHouseholdOrders 
  $ mapWhere (not . householdOrderIsAbandoned) 
  $ applyUpdate catalogue

acceptCatalogueUpdate :: HouseholdId -> Order -> Order
acceptCatalogueUpdate householdId =
    overHouseholdOrders
  $ mapWhere ((== householdId) . householdOrderHouseholdId)
  $ acceptUpdate

reconcileOrderItems :: UTCTime -> [(HouseholdId, OrderItemSpec)] -> Order -> Order
reconcileOrderItems date updates = 
    overHouseholdOrders (map reconcile)
  where
    reconcile ho = let updatesForHousehold = map snd . filter (((==) (householdOrderHouseholdId ho)) . fst) $ updates
                   in  reconcileItems date updatesForHousehold ho

data OrderItemSpec = OrderItemSpec
  { _itemSpecProductCode :: ProductCode
  , _itemSpecProductPrice :: Int
  , _itemSpecQuantity :: Int
  } deriving (Eq, Show, Generic)

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

householdOrderHouseholdId :: HouseholdOrder -> HouseholdId
householdOrderHouseholdId = _householdId . _householdOrderHouseholdInfo

householdOrderHouseholdName :: HouseholdOrder -> String
householdOrderHouseholdName = _householdName . _householdOrderHouseholdInfo

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

householdOrderIsOpen :: HouseholdOrder -> Bool
householdOrderIsOpen ho = (not (householdOrderIsComplete ho) && not (householdOrderIsAbandoned ho) && not (householdOrderIsPlaced ho))

householdOrderIsReconciled :: HouseholdOrder -> Bool
householdOrderIsReconciled ho = householdOrderIsPlaced ho && (all (isJust . _itemAdjustment) . _householdOrderItems $ ho)

householdOrderIsAwaitingCatalogueUpdateConfirm :: HouseholdOrder -> Bool
householdOrderIsAwaitingCatalogueUpdateConfirm ho = 
     not (householdOrderIsAbandoned ho)
  && not (householdOrderIsPlaced ho)
  && any (isJust . _itemAdjustment) (_householdOrderItems ho)

overHouseholdOrderItems :: ([OrderItem] -> [OrderItem]) -> HouseholdOrder -> HouseholdOrder
overHouseholdOrderItems fn ho = ho{ _householdOrderItems = fn $ _householdOrderItems $ ho }

abandon :: HouseholdOrder -> HouseholdOrder
abandon ho = 
  ho { _householdOrderStatusFlags = HouseholdOrderStatusFlags { _householdOrderIsAbandoned = True
                                                              , _householdOrderIsPlaced = False 
                                                              , _householdOrderIsComplete = False
                                                              } 
     }

complete :: HouseholdOrder -> HouseholdOrder
complete ho = 
  ho { _householdOrderStatusFlags = HouseholdOrderStatusFlags { _householdOrderIsAbandoned = False
                                                              , _householdOrderIsPlaced = False 
                                                              , _householdOrderIsComplete = True
                                                              } 
     }

reopen :: HouseholdOrder -> HouseholdOrder
reopen ho =
  ho { _householdOrderStatusFlags = HouseholdOrderStatusFlags { _householdOrderIsAbandoned = False
                                                              , _householdOrderIsPlaced = False 
                                                              , _householdOrderIsComplete = False
                                                              } }

addOrUpdateItems :: ProductCatalogue -> [(ProductCode, Maybe Int)] -> HouseholdOrder -> HouseholdOrder
addOrUpdateItems catalogue itemQuantities = 
    overHouseholdOrderItems 
  $ addOrUpdate (\(code, _) i -> code == itemProductCode i)
                (\(code, quantity) -> findEntry code catalogue <&> \e -> OrderItem (fromCatalogueEntry e) (fromMaybe 1 quantity) Nothing)
                (\(_, quantity) i -> updateItemQuantity quantity i)
                itemQuantities

removeItem :: ProductCode -> HouseholdOrder -> HouseholdOrder
removeItem productCode = 
    overHouseholdOrderItems 
  $ filter ((/= productCode) . itemProductCode)

applyUpdate :: ProductCatalogue -> HouseholdOrder -> HouseholdOrder
applyUpdate catalogue = 
    overHouseholdOrderItems 
  $ map apply
  where
    date = getDate catalogue
    apply item = case findEntry (itemProductCode item) catalogue of
      Just e | itemProductPrice item == _catalogueEntryPrice e -> item
             | otherwise -> adjustItemPrice date (_catalogueEntryPrice e) item
      _ -> discontinueProduct date item

acceptUpdate :: HouseholdOrder -> HouseholdOrder
acceptUpdate = 
    overHouseholdOrderItems 
  $ updateOrRemove accept
  where
    accept  (OrderItem { _itemAdjustment = Just (OrderItemAdjustment { _itemAdjIsDiscontinued = True }) }) = Nothing
    accept i@OrderItem { _itemAdjustment = Just (OrderItemAdjustment { _itemAdjNewPrice = price
                                                                     , _itemAdjNewQuantity = quantity
                                                                     })
                        } = Just $ updateItemPrice price
                                 $ updateItemQuantity (Just quantity)
                                 $ removeItemAdjustment i
    accept i = Just i

reconcileItems :: UTCTime -> [OrderItemSpec] -> HouseholdOrder -> HouseholdOrder
reconcileItems date updates = 
    overHouseholdOrderItems 
  $ map update
  where
    update i = 
      case find ((== itemProductCode i) . _itemSpecProductCode) updates of
        Just spec -> adjustItemPrice date (reprice (_itemSpecProductPrice spec) (itemProductPrice i))
                   $ adjustItemQuantity date (_itemSpecQuantity spec) i
        _ -> i

{- OrderItem -}

data OrderItem = OrderItem  
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

itemProductCode :: OrderItem -> ProductCode
itemProductCode = _productCode . _productInfo . _itemProduct

itemProductName :: OrderItem -> String
itemProductName = _productName . _productInfo . _itemProduct

itemProductPrice :: OrderItem -> Price
itemProductPrice = _productPrice . _productInfo . _itemProduct

itemTotal :: OrderItem -> Money
itemTotal item = price * quantity
  where
    price = _priceAmount . itemProductPrice $ item
    quantity = fromIntegral $ _itemQuantity item

updateItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateItemQuantity quantity i = i{ _itemQuantity = fromMaybe (_itemQuantity i) quantity }

updateItemPrice :: Price -> OrderItem -> OrderItem
updateItemPrice price i@OrderItem { _itemProduct = p@Product { _productInfo = pi } } = 
  i{ _itemProduct = p{ _productInfo = pi{ _productPrice = price } } }

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
adjustItemQuantiity date quantity i = 
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
removeItemAdjustment i@OrderItem { _itemAdjustment = Just a } = i{ _itemAdjustment = Nothing }
removeItemAdjustment i = i

{- Product -}

newtype ProductId = ProductId
  { fromProductId :: Int 
  } deriving (Eq, Show, Generic)

newtype ProductCode = ProductCode
  { fromProductCode :: String 
  } deriving (Eq, Show, Generic)

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
atVatRate vatRate amountExcVat = Price vatRate $
  Money amountExcVat $ round $ fromIntegral amountExcVat * (_vatRateMultiplier  vatRate)

atQuantity :: Int -> Price -> Money
atQuantity quantity price = (_priceAmount price * fromIntegral quantity)

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

newtype ProductCatalogue = ProductCatalogue { fromProductCatalogue :: H.HashMap ProductCode ProductCatalogueEntry }

productCatalogue :: [ProductCatalogueEntry] -> ProductCatalogue
productCatalogue = ProductCatalogue . H.fromList . map (_catalogueEntryCode &&& id)

findEntry :: ProductCode -> ProductCatalogue -> Maybe ProductCatalogueEntry
findEntry code = H.lookup code . fromProductCatalogue

getEntries :: ProductCatalogue -> [ProductCatalogueEntry]
getEntries = H.elems . fromProductCatalogue

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
    parse [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
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
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
                            
justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing

addOrUpdate :: (a -> b -> Bool) -> (a -> Maybe b) -> (a -> b -> b) -> [a] -> [b] -> [b]
addOrUpdate _ _ _ [] ys = ys
addOrUpdate eq add update (x:xs) ys = 
  case partition (eq x) ys of
    (y:_, ys') -> (update x y):addOrUpdate eq add update xs ys' 
    _ -> case add x of
      Just y -> y:addOrUpdate eq add update xs ys
      _ -> addOrUpdate eq add update xs ys

updateOrRemove :: (a -> Maybe a) -> [a] -> [a]
updateOrRemove _ [] = []
updateOrRemove fn (x:xs) = case fn x of
  Just x' -> x':(updateOrRemove fn xs)
  _ -> updateOrRemove fn xs

mapWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhere _ _ [] = []
mapWhere cond fn (x:xs) = (if cond x then fn x else x):mapWhere cond fn xs

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>