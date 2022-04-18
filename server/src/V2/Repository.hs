{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module V2.Repository where 

import           Control.Arrow ((***), (&&&))
import           Control.Monad (when, join, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.ByteString (ByteString)
import           Data.Function (on)
import           Data.List (deleteFirstsBy, intersectBy, nub)
import qualified Data.Map as M (fromList, elems)
import           Data.Maybe (listToMaybe, maybeToList, catMaybes)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Database.PostgreSQL.Simple (Connection, Only(..), (:.)(..), connectPostgreSQL, close, withTransaction)

import V2.Domain
import V2.Repository.SQL

data RepositoryConfig = RepositoryConfig 
  { repoConnectionString :: ByteString
  , repoGroupKey :: String 
  }
  
data Repository = Repository { connection :: Connection }

connect :: RepositoryConfig -> ((Repository, OrderGroupId) -> MaybeT IO a) -> MaybeT IO a
connect config action = do
  conn <- liftIO $ connectPostgreSQL $ repoConnectionString $ config
  groupId <- MaybeT $ listToMaybe . fmap fromOnly <$> (liftIO $ selectOrderGroupId conn $ repoGroupKey config)
  result <- MaybeT $ liftIO $ withTransaction conn $ runMaybeT $ action (Repository conn, groupId)
  liftIO $ close conn
  return result

getOrderGroup :: Repository -> OrderGroupId -> IO OrderGroup
getOrderGroup repo groupId = do
  let conn = connection repo
  head <$> selectOrderGroup conn groupId

getVatRates :: Repository -> IO [VatRate]
getVatRates repo = do
  let conn = connection repo
  selectVatRates conn

getProductCatalogueFile :: Repository -> IO (Maybe (UTCTime, Text))
getProductCatalogueFile repo = do
  let conn = connection repo

  listToMaybe <$> selectProductCatalogueFile conn

setProductCatalogueFile :: Repository -> UTCTime -> Text -> IO ()
setProductCatalogueFile repo date file = do
  let conn = connection repo

  insertProductCatalogueFile conn date file

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
getProductCode :: Repository -> ProductId -> IO (Maybe ProductCode)
getProductCode repo productId = do
  let conn = connection repo

  liftM fst <$> listToMaybe <$> selectProducts conn [ForProductId productId]

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
getProductIdsForCurrentOrder :: Repository -> OrderGroupId -> IO [(ProductCode, ProductId)]
getProductIdsForCurrentOrder repo groupId = do
  let conn = connection repo

  selectProducts conn [ForOrderGroup groupId, OrderIsCurrent]

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
getProductIdsForPastOrders :: Repository -> OrderGroupId -> IO [(ProductCode, ProductId)]
getProductIdsForPastOrders repo groupId = do
  let conn = connection repo

  selectProducts conn [ForOrderGroup groupId, OrderIsPast]

getProductData :: Repository -> ProductCode -> IO (Maybe (Maybe ByteString, Maybe String, Maybe String, Maybe String, Maybe Int))
getProductData repo code = do
  let conn = connection repo  
  productData <- selectProductData conn code
  return $ listToMaybe $ productData

setProductData :: Repository -> ProductCode -> Maybe ByteString -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> IO ()
setProductData repo code image url title imageUrl size = do
  let conn = connection repo
  
  insertProductData conn code image url title imageUrl size

getHouseholds :: Repository -> Maybe OrderGroupId -> IO [Household]
getHouseholds repo groupId = do
  let conn = connection repo

  let params = ForOrderGroup <$> maybeToList groupId
  (rHouseholds, rHouseholdOrders, rOrderItems, rPayments) <- do
    rHouseholds <- selectHouseholdRows conn params 
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    rPayments <- selectPayments conn params
    return (rHouseholds, rHouseholdOrders, rOrderItems, rPayments)
  return $ map (toHousehold rHouseholdOrders rOrderItems rPayments) rHouseholds

getHousehold :: Repository -> OrderGroupId -> HouseholdId -> IO (Maybe Household)
getHousehold repo groupId householdId = do
  let conn = connection repo

  let params = [ForOrderGroup groupId, ForHousehold householdId]
  (rHouseholds, rHouseholdOrders, rOrderItems, rPayments) <- do
    rHouseholds <- selectHouseholdRows conn params 
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    rPayments <- selectPayments conn params
    return (rHouseholds, rHouseholdOrders, rOrderItems, rPayments)
  return $ listToMaybe $ map (toHousehold rHouseholdOrders rOrderItems rPayments) rHouseholds

getHouseholdInfo :: Repository -> OrderGroupId -> HouseholdId -> IO (Maybe HouseholdInfo)
getHouseholdInfo repo groupId householdId = do
  let conn = connection repo

  let params = [ForOrderGroup groupId, ForHousehold householdId]
  listToMaybe . (map householdRow_householdInfo) <$> selectHouseholdRows conn params 

createHousehold :: Repository -> OrderGroupId -> HouseholdSpec -> IO HouseholdId
createHousehold repo groupId spec = do
  let conn = connection repo

  [Only id] <- insertHousehold conn groupId spec
  return id

setHousehold :: Repository -> OrderGroupId -> (Household, Household) -> IO ()
setHousehold repo groupId (household, household') = do
  let conn = connection repo

  when (_householdInfo household' /= _householdInfo household 
     || _householdContact household' /= _householdContact household)
    (updateHouseholds conn groupId [household'])

removeHousehold :: Repository -> OrderGroupId -> HouseholdId -> IO ()
removeHousehold repo groupId householdId = do
  let conn = connection repo

  deleteHousehold conn groupId householdId

getPayment :: Repository -> OrderGroupId -> PaymentId -> IO (Maybe Payment)
getPayment repo groupId paymentId = do
  let conn = connection repo

  let params = [ForOrderGroup groupId, ForPayment paymentId]
  listToMaybe <$> selectPayments conn params

getPayments :: Repository -> Maybe OrderGroupId -> IO [Payment]
getPayments repo groupId = do
  let conn = connection repo

  let params = ForOrderGroup <$> maybeToList groupId
  selectPayments conn params

createPayment :: Repository -> OrderGroupId -> PaymentSpec -> IO PaymentId
createPayment repo groupId spec = do
  let conn = connection repo

  [Only id] <- insertPayment conn groupId spec
  return id

setPayment :: Repository -> OrderGroupId -> (Payment, Payment) -> IO ()
setPayment repo groupId (payment, payment') = do
  let conn = connection repo

  when (payment' /= payment) (updatePayments conn groupId [payment'])

removePayment :: Repository -> OrderGroupId -> PaymentId -> IO ()
removePayment repo groupId paymentId = do
  let conn = connection repo

  deletePayment conn groupId paymentId

getOrder :: Repository -> OrderGroupId -> OrderId -> IO (Maybe Order)
getOrder repo groupId orderId = do
  let conn = connection repo

  let params = [ForOrderGroup groupId, ForOrder orderId]
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn params
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getCurrentOrder :: Repository -> OrderGroupId -> IO (Maybe Order)
getCurrentOrder repo groupId = do
  let conn = connection repo

  let params = [ForOrderGroup groupId, OrderIsCurrent]
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn params
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getCurrentOrders :: Repository -> Maybe OrderGroupId -> IO [Order]
getCurrentOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) ++ [OrderIsCurrent]
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn params
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getPastOrders :: Repository -> Maybe OrderGroupId -> IO [Order]
getPastOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) ++ [OrderIsPast]
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn params 
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

createOrder :: Repository -> OrderGroupId -> OrderSpec -> IO OrderId
createOrder repo groupId spec = do
  let conn = connection repo

  [Only id] <- insertOrder conn groupId spec
  return id

setOrders :: Repository -> ([Order], [Order]) -> IO ()
setOrders repo orders = do
    let conn = connection repo

    updateOrders conn $ updatedByComparing orderKey _orderStatus orders

    let householdOrders = join (***) (concatMap _orderHouseholdOrders) $ orders
    setHouseholdOrders repo householdOrders
  where
    orderKey o = let groupId = _orderGroupId . _orderInfo $ o
                     orderId = _orderId . _orderInfo $ o
                 in (groupId, orderId)

getHouseholdOrder :: Repository -> OrderGroupId -> OrderId -> HouseholdId -> IO (Maybe HouseholdOrder)
getHouseholdOrder repo groupId orderId householdId = do
  let conn = connection repo
  
  let params = [ForOrderGroup groupId, ForOrder orderId, ForHousehold householdId]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

getCurrentHouseholdOrders :: Repository -> Maybe OrderGroupId -> IO [HouseholdOrder]
getCurrentHouseholdOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) ++ [OrderIsCurrent]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

getPastHouseholdOrders :: Repository -> Maybe OrderGroupId -> IO [HouseholdOrder]
getPastHouseholdOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) ++ [OrderIsPast]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

setHouseholdOrders :: Repository -> ([HouseholdOrder], [HouseholdOrder]) -> IO ()
setHouseholdOrders repo orders = do
    let conn = connection repo
    products <- map fst <$> selectProducts conn []

    let orderKey o = let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                         orderId = _orderId . _householdOrderOrderInfo $ o
                         householdId = _householdId . _householdOrderHouseholdInfo $ o
                     in (groupId, orderId, householdId)

    let itemKey ((groupId, orderId, householdId), i) = ((groupId, orderId, householdId, itemProductCode i), i)
    let keyedItems o = map itemKey . map (orderKey o, ) . M.elems . _householdOrderItems $ o
    let keyedAdjustment (k, i) = (k, ) <$> _itemAdjustment i

    let items = join (***) (concatMap keyedItems) $ orders
    let products' = (products, nub . map (itemProductCode . snd) . snd $ items)
    let adjustments = join (***) (catMaybes . map keyedAdjustment) $ items

    -- TODO?
    -- let removedOrders  = removedBy orderKey orders
    let addedOrders = addedBy orderKey orders
    let updatedOrders = updatedByComparing orderKey _householdOrderStatus orders

    let addedItems = addedBy fst items
    let updatedItems = updatedByComparing fst snd items
    let removedItems = removedBy fst items

    let addedAdjustments = addedBy fst adjustments
    let updatedAdjustments = updatedByComparing fst snd adjustments
    let removedAdjustments = removedBy fst adjustments

    let addedProducts = addedBy id products'

    insertHouseholdOrders conn $ addedOrders
    updateHouseholdOrders conn $ updatedOrders
    insertProducts conn $ addedProducts
    insertOrderItems conn $ addedItems
    insertOrderItemAdjustments conn $ addedAdjustments
    deleteOrderItemAdjustments conn $ removedAdjustments
    deleteOrderItems conn $ removedItems
    updateOrderItems conn $ updatedItems
    updateOrderItemAdjustments conn $ updatedAdjustments

    return ()

getFileUpload :: Repository -> OrderGroupId -> String -> IO (Maybe ByteString)
getFileUpload repo groupId fileId = do
  let conn = connection repo

  listToMaybe <$> fmap fromOnly <$> selectFileUpload conn groupId fileId

setFileUpload :: Repository -> OrderGroupId -> String -> ByteString -> IO ()
setFileUpload repo groupId fileId contents =  do
  let conn = connection repo

  upsertFileUpload conn groupId fileId contents

removeFileUpload :: Repository -> OrderGroupId -> String -> IO ()
removeFileUpload repo groupId fileId =  do
  let conn = connection repo

  deleteFileUpload conn groupId fileId

toHousehold :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> [Payment] -> (HouseholdRow) -> Household
toHousehold rHouseholdOrders rOrderItems rPayments h = 
  Household householdInfo
            householdContact
            householdOrders
            householdPayments
  where
  householdInfo = householdRow_householdInfo h
  householdContact = householdRow_contact h
  householdOrders = map (toHouseholdOrder rOrderItems)
                  . filter (( == _householdId householdInfo) . _householdId . householdOrderRow_householdInfo)
                  $ rHouseholdOrders      
  householdPayments = filter (( == _householdId householdInfo) . _paymentHouseholdId)
                    $ rPayments

toOrder :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> OrderRow -> Order
toOrder rHouseholdOrders rOrderItems o = 
  Order orderInfo
        (orderRow_status o)
        householdOrders
  where
  orderInfo = orderRow_orderInfo o
  householdOrders = map (toHouseholdOrder rOrderItems)
                  . filter ((== _orderId orderInfo) . _orderId . householdOrderRow_orderInfo)
                  $ rHouseholdOrders

toHouseholdOrder :: [(OrderId, HouseholdId) :. OrderItemRow] -> HouseholdOrderRow -> HouseholdOrder
toHouseholdOrder rOrderItems ho = 
  HouseholdOrder orderInfo
                 orderStatus
                 householdInfo
                 (householdOrderRow_status ho)
                 items
  where
  orderInfo = householdOrderRow_orderInfo ho
  orderStatus = householdOrderRow_orderStatus ho
  householdInfo = householdOrderRow_householdInfo ho
  items = M.fromList
        . map (itemProductCode &&& id)
        . map toOrderItem 
        . filter (\((oId, hId) :. _) -> oId == _orderId orderInfo && hId == _householdId householdInfo)
        $ rOrderItems

toOrderItem :: ((OrderId, HouseholdId) :. OrderItemRow) -> OrderItem
toOrderItem (_ :. i) = OrderItem (orderItemRow_product i)
                                 (orderItemRow_quantity i)
                                 (orderItemRow_adjustment i)
                                 (orderItemRow_packed i)

addedBy :: Eq b => (a -> b) -> ([a], [a]) -> [a]
addedBy   key (xs, xs') = deleteFirstsBy ((==) `on` key) xs' xs

removedBy :: Eq b => (a -> b) -> ([a], [a]) -> [a]
removedBy key (xs, xs') = deleteFirstsBy ((==) `on` key) xs  xs'

updatedByComparing :: (Eq b, Eq c) => (a -> b) -> (a -> c) -> ([a], [a]) -> [a]
updatedByComparing key compare (xs, xs') = map snd
                                         . filter (uncurry (/=) . join (***) compare)
                                         $ (intersectBy ((==) `on` key) xs  xs')
                                           `zip`
                                           (intersectBy ((==) `on` key) xs' xs)