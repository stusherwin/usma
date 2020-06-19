{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module RepositoryV2 where 

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Monad (mzero, forM_, void, when, join, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (find, foldl', deleteFirstsBy, intersectBy, nub)
import Data.Maybe (listToMaybe, fromMaybe, maybeToList, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only(..), Query, (:.)(..), connectPostgreSQL, close, withTransaction, query, query_, execute, execute_, executeMany)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), RowParser, field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.SqlQQ
import Prelude hiding (sum)

import Config (Config(..))
import DomainV2

data RepositoryConfig = RepositoryConfig { repoConnectionString :: ByteString, repoGroupKey :: String }
data Repository = Repository { connection :: Connection }

connect :: RepositoryConfig -> ((Repository, OrderGroupId) -> MaybeT IO a) -> MaybeT IO a
connect config action = do
  conn <- liftIO $ connectPostgreSQL $ repoConnectionString $ config
  (maybeGroupId :: [Only Int]) <- liftIO $ query conn [sql|
    select id
    from order_group
    where key = ?
  |] (Only $ repoGroupKey config)
  connection <- MaybeT . return $ (Repository conn, ) . OrderGroupId . fromOnly <$> listToMaybe maybeGroupId
  result <- MaybeT $ liftIO $ withTransaction conn $ runMaybeT $ action connection
  liftIO $ close conn
  return result

getVatRates :: Repository -> IO [VatRate]
getVatRates repo = do
  let conn = connection repo
  selectVatRates conn

getProductCatalogue :: Repository -> IO ProductCatalogue
getProductCatalogue repo = do
  let conn = connection repo

  productCatalogue <$> selectCatalogueEntries conn []

getProductCatalogueForCode :: Repository -> ProductCode -> IO ProductCatalogue
getProductCatalogueForCode repo code = do
  let conn = connection repo

  productCatalogue <$> selectCatalogueEntries conn [ForProductCode code]

getProductCatalogueForOrder :: Repository -> OrderGroupId -> OrderId -> IO ProductCatalogue
getProductCatalogueForOrder repo groupId orderId = do
  let conn = connection repo

  productCatalogue <$> selectCatalogueEntries conn [ForOrderGroup groupId, ForOrder orderId]

getProductCatalogueCategories :: Repository -> IO [String]
getProductCatalogueCategories repo = do
  let conn = connection repo

  selectCatalogueEntryCategories conn

getProductCatalogueBrands :: Repository -> IO [String]
getProductCatalogueBrands repo = do
  let conn = connection repo

  selectCatalogueEntryBrands conn

setProductCatalogue :: Repository -> ProductCatalogue -> IO ()
setProductCatalogue repo catalogue = do
  let conn = connection repo

  truncateCatalogueEntries conn
  insertCatalogueEntries conn (getEntries catalogue)

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

  let params = (ForOrderGroup <$> maybeToList groupId) <> [OrderIsCurrent]
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn params
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getPastOrders :: Repository -> Maybe OrderGroupId -> IO [Order]
getPastOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) <> [OrderIsPast]
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

    updateOrders conn $ updatedByComparing orderKey _orderStatusFlags orders

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

  let params = (ForOrderGroup <$> maybeToList groupId) <> [OrderIsCurrent]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

getPastHouseholdOrders :: Repository -> Maybe OrderGroupId -> IO [HouseholdOrder]
getPastHouseholdOrders repo groupId = do
  let conn = connection repo

  let params = (ForOrderGroup <$> maybeToList groupId) <> [OrderIsPast]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

setHouseholdOrders :: Repository -> ([HouseholdOrder], [HouseholdOrder]) -> IO ()
setHouseholdOrders repo orders = do
    let conn = connection repo

    -- TODO?
    -- let removedOrders  = removedBy orderKey orders
    insertHouseholdOrders conn $ addedBy   orderKey orders
    updateHouseholdOrders conn $ updatedByComparing orderKey _householdOrderStatusFlags orders

    let items = join (***) (concat . map keyedItems) $ orders
    let products = join (***) (nub . map (itemProductCode . snd)) $ items
    let adjustments = join (***) (catMaybes . map keyedAdjustment) $ items

    insertProducts conn $ addedBy id products

    insertOrderItems conn $ addedBy fst items
    insertOrderItemAdjustments conn $ addedBy fst adjustments

    deleteOrderItemAdjustments conn $ removedBy fst adjustments
    deleteOrderItems conn $ removedBy fst items
    
    updateOrderItems conn $ updatedByComparing fst snd items
    updateOrderItemAdjustments conn $ updatedByComparing fst snd adjustments

    return ()
  where
    orderKey o = let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                     orderId = _orderId . _householdOrderOrderInfo $ o
                     householdId = _householdId . _householdOrderHouseholdInfo $ o
                     status = householdOrderStatus o
                 in (groupId, orderId, householdId, status)
    keyedItems o = map (orderKey o, ) $ _householdOrderItems o
    itemKey ((groupId, orderId, householdId, status), i) = (groupId, orderId, householdId, status, itemProductCode i)
    keyedAdjustment i = (itemKey i, ) <$> (_itemAdjustment . snd) i

selectVatRates :: Connection -> IO [VatRate]
selectVatRates conn = 
  query_ conn ([sql|
    select code, multiplier
    from v2.vat_rate
  |])

selectCatalogueEntries :: Connection -> [WhereParam] -> IO [ProductCatalogueEntry]
selectCatalogueEntries conn whereParams = 
    query conn ([sql|
      select ce.code
           , ce.category
           , ce.brand
           , ce."description"
           , ce."text"
           , ce.size
           , ce.price
           , ce.vat_rate
           , v.multiplier
           , ce.rrp
           , ce.biodynamic
           , ce.fair_trade
           , ce.gluten_free
           , ce.organic
           , ce.added_sugar
           , ce.vegan
           , ce.updated
      from v2.catalogue_entry ce
      left join v2.vat_rate v on v.code = ce.vat_rate
      left join v2.order_item oi on oi.product_code = ce.code
      left join v2."order" o on oi.order_id = o.id
      where 1 = 1 |] <> whereClause <> [sql|
      order by ce.code
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForProductCode _) -> Just [sql| ce.code = ? |]
      (ForOrder _) -> Just [sql| o.id = ? |]
      (ForOrderGroup _) -> Just [sql| o.order_group_id = ? |]
      _ -> Nothing

selectCatalogueEntryCategories :: Connection -> IO [String]
selectCatalogueEntryCategories conn = 
    query_ conn [sql|
      select distinct ce.category
      from v2.catalogue_entry ce
      order by ce.category
    |]

selectCatalogueEntryBrands :: Connection -> IO [String]
selectCatalogueEntryBrands conn = 
    query_ conn [sql|
      select distinct ce.brand
      from v2.catalogue_entry ce
      order by ce.category
    |]

truncateCatalogueEntries :: Connection -> IO ()
truncateCatalogueEntries conn =   
  void $ execute_ conn [sql|
    truncate table catalogue_entry
  |]

insertCatalogueEntries :: Connection -> [ProductCatalogueEntry] -> IO ()
insertCatalogueEntries conn entries = 
  void $ executeMany conn [sql|
    insert into v2.catalogue_entry 
      ( "code"
      , category
      , brand
      , "description"
      , "text"
      , size
      , price
      , vat_rate
      , rrp
      , biodynamic
      , fair_trade
      , gluten_free
      , organic
      , added_sugar
      , vegan
      , updated
      )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] entries

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
selectProducts :: Connection -> [WhereParam] -> IO [(ProductCode, ProductId)]
selectProducts conn whereParams = 
    query conn ([sql|
      select p.code
           , p.id
      from v2.product p 
      join v2.order_item oi on oi.product_code = p.code
      join v2."order" o on oi.order_id = o.id
      where 1 = 1 |] <> whereClause <> [sql|
      order by p.code
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForProductCode _)     -> Just [sql| p.code = ? |]
      (ForProductId _)   -> Just [sql| p.id = ? |]
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      (ForOrder _)       -> Just [sql| oi.id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      _ -> Nothing

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
insertProducts :: Connection -> [ProductCode] -> IO ()
insertProducts conn codes =
  void $ executeMany conn [sql|
    insert into v2.product 
      ( "code"
      )
    values (?)
    on conflict ("code") do nothing
  |] $ map Only codes

selectHouseholdRows :: Connection -> [WhereParam] -> IO [HouseholdRow]
selectHouseholdRows conn whereParams = 
    query conn ([sql|
      select h.id
           , h.name
           , h.contact_name
           , h.contact_email
           , h.contact_phone
      from household h
      where h.archived = false |] <> whereClause <> [sql| 
      order by h.name asc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| h.order_group_id = ? |]
      (ForHousehold _)  -> Just [sql| h.id = ? |]
      _ -> Nothing

insertHousehold :: Connection -> OrderGroupId -> HouseholdSpec -> IO [Only HouseholdId]
insertHousehold conn groupId spec = 
  query conn [sql|
    insert into v2.household 
      ( order_group_id
      , name
      , contact_name
      , contact_email
      , contact_phone
      , is_archived
      ) 
    values (?, ?, ?, ?, ?, false)
    returning id
  |] ( groupId
     , _householdSpecName spec
     , _contactName . _householdSpecContact $ spec
     , _contactEmail . _householdSpecContact $ spec
     , _contactPhone . _householdSpecContact $ spec
     )

updateHouseholds :: Connection -> OrderGroupId -> [Household] -> IO ()
updateHouseholds conn groupId households = do
  let rows = households <&> \h -> ( _householdName . _householdInfo $ h
                                  , _contactName . _householdContact $ h
                                  , _contactEmail . _householdContact $ h
                                  , _contactPhone . _householdContact $ h
                                  , _householdId . _householdInfo $ h
                                  , groupId
                                  )
  void $ executeMany conn [sql|
    update v2.household 
    set name = ?
      , contact_name = ?
      , contact_email = ?
      , contact_phone = ?
    where id = ?
      and order_group_id = ?
  |] rows

deleteHousehold :: Connection -> OrderGroupId -> HouseholdId -> IO ()
deleteHousehold conn groupId householdId = 
  void $ execute conn [sql|
    update v2.household
    set is_archived = true
    where id = ?
      and order_group_id = ?
  |] ( householdId
     , groupId
     )

selectPayments :: Connection -> [WhereParam] -> IO [Payment]
selectPayments conn whereParams = 
    query conn ([sql|
      select p.id
           , p.household_id
           , p."date"
           , p.amount
      from household_payment p
      where p.archived = false |] <> whereClause <> [sql|
      order by p.id asc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| p.order_group_id = ? |]
      (ForHousehold _)  -> Just [sql| p.household_id = ? |]
      (ForPayment _)  -> Just [sql| p.id = ? |]
      _ -> Nothing

insertPayment :: Connection -> OrderGroupId -> PaymentSpec -> IO [Only PaymentId]
insertPayment conn groupId spec = 
  query conn [sql|
    insert into v2.payment 
      ( order_group_id
      , household_id
      , "date"
      , amount
      , is_archived
      ) 
    values (?, ?, ?, ?, false)
    returning id
  |] ( groupId
     , _paymentSpecHouseholdId spec
     , _paymentSpecDate spec
     , _paymentSpecAmount spec
     )

updatePayments :: Connection -> OrderGroupId -> [Payment] -> IO ()
updatePayments conn groupId payments = do
  let rows = payments <&> \p -> ( _paymentDate p
                                , _paymentAmount p
                                , _paymentId p
                                , groupId
                                )
  void $ executeMany conn [sql|
    update v2.payment 
    set "date" = ?
      , amount = ?
    where id = ?
      and order_group_id = ?
  |] rows

deletePayment :: Connection -> OrderGroupId -> PaymentId -> IO ()
deletePayment conn groupId paymentId = 
  void $ execute conn [sql|
    update v2.payment 
    set is_archived = true
    where id = ?
      and order_group_id = ?
  |] ( paymentId
     , groupId
     )

selectOrderRows :: Connection -> [WhereParam] -> IO [OrderRow]
selectOrderRows conn whereParams = do
    query conn ([sql|
      select o.id
           , o.order_group_id
           , o.created
           , cb.id as created_by_household_id
           , cb.name as created_by_household_name
      from v2."order" o
      left join v2.household cb
        on cb.id = o.created_by_id 
      where 1 = 1 |] <> whereClause <> [sql|
      order by o.id desc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder orderId) -> Just [sql| o.id = ? |]
      _ -> Nothing

insertOrder :: Connection -> OrderGroupId -> OrderSpec -> IO [Only OrderId]
insertOrder conn groupId spec = 
  query conn [sql|
    insert into v2."order" (order_group_id, created_date, created_by_id) 
    values (?, ?, ?) 
    returning id
  |] (groupId, _orderSpecCreated spec, _orderSpecCreatedByHouseholdId spec)

updateOrders :: Connection -> [Order] -> IO ()
updateOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _orderInfo $ o
                                  orderId = _orderId . _orderInfo $ o
                                  abandoned = orderIsAbandoned o
                                  placed = orderIsPlaced o
                                  --complete?
                              in (abandoned, placed, {- complete?, -} groupId, orderId)
  void $ executeMany conn [sql|
    update v2."order"
    set is_abandoned = ? 
      , is_placed = ?
    where order_group_id = ? and order_id = ?
  |] rows

selectHouseholdOrderRows :: Connection -> [WhereParam] -> IO [HouseholdOrderRow]
selectHouseholdOrderRows conn whereParams = 
    query conn ([sql|
      select o.id as order_id 
           , o.order_group_id
           , o.created
           , cb.id as created_by_household_id
           , cb.name as created_by_household_name
           , h.id as household_id
           , h.name as household_name
           , ho.is_abandoned
           , ho.is_placed
           , ho.is_complete
      from v2.household_order ho
      inner join v2."order" o 
        on o.id = ho.order_id
      left join v2.household cb
        on cb.id = o.created_by_id 
      inner join v2.household h 
        on h.id = ho.household_id
      where 1 = 1 |] <> whereClause <> [sql|
      order by o.id desc, h.name asc
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      (ForHousehold _)   -> Just [sql| h.id = ? |]

insertHouseholdOrders :: Connection -> [HouseholdOrder] -> IO ()
insertHouseholdOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                                  orderId = _orderId . _householdOrderOrderInfo $ o
                                  householdId = _householdId . _householdOrderHouseholdInfo $ o
                                  abandoned = householdOrderIsAbandoned o
                                  complete = householdOrderIsComplete o
                                  placed = householdOrderIsPlaced o
                              in (abandoned, complete, placed, groupId, orderId, householdId)
  void $ executeMany conn [sql|
    insert into v2.household_order (is_abandoned, is_complete, is_placed, order_group_id, order_id, household_id) 
    values (?, ?, ?, ?, ?, ?)
    on conflict (order_group_id, order_id, household_id) do nothing
  |] rows

updateHouseholdOrders :: Connection -> [HouseholdOrder] -> IO ()
updateHouseholdOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                                  orderId = _orderId . _householdOrderOrderInfo $ o
                                  householdId = _householdId . _householdOrderHouseholdInfo $ o
                                  abandoned = householdOrderIsAbandoned o
                                  complete = householdOrderIsComplete o
                                  placed = householdOrderIsPlaced o
                              in (abandoned, complete, placed, groupId, orderId, householdId)
  void $ executeMany conn [sql|
    update v2.household_order 
    set is_abandoned = ? 
      , is_complete = ?
      , is_placed = ?
    where order_group_id = ? and order_id = ? and household_id = ?
  |] rows

selectOrderItemRows :: Connection -> [WhereParam] -> IO [(OrderId, HouseholdId) :. OrderItemRow]
selectOrderItemRows conn whereParams = 
    query conn ([sql|
      select hoi.order_id
           , hoi.household_id
           , hoi.product_code
           , hoi.product_name
           , hoi.product_vat_rate
           , hoi.product_vat_rate_multiplier
           , hoi.product_price
           , hoi.product_is_biodynamic
           , hoi.product_is_fair_trade
           , hoi.product_is_gluten_free
           , hoi.product_is_organic
           , hoi.product_is_added_sugar
           , hoi.product_is_vegan
           , hoi.quantity
           , adj.new_vat_rate 
           , adjv.multiplier
           , adj.new_price
           , adj.new_quantity
           , adj.is_discontinued
           , adj.date
      from v2.order_item hoi
      inner join v2.household_order ho 
        on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
      inner join v2."order" o
        on ho.order_id = o.id
      left join v2.product p
        on hoi.product_code = p.code
      left join v2.order_item_adjustment adj
        on hoi.adjustment_id adj.id
      left join v2.vat_rate adjv
        on adjv.code = adj.new_vat_rate
      where 1 = 1 |] <> whereClause <> [sql|
      order by hoi.id
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      (ForHousehold _)   -> Just [sql| ho.household_id = ? |]

insertOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
insertOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, _), i) -> 
                         UpdateOrderItem (_itemQuantity i)
                                         (_productName . _productInfo . _itemProduct $ i)
                                         (_moneyExcVat . _priceAmount . itemProductPrice $ i)
                                         (_vatRateType . _priceVatRate . itemProductPrice $ i)
                                         ((realToFrac $ _vatRateMultiplier . _priceVatRate . itemProductPrice $ i) :: Double)
                                         (_productIsBiodynamic . _productFlags . _itemProduct $ i)
                                         (_productIsFairTrade  . _productFlags . _itemProduct $ i)
                                         (_productIsGlutenFree . _productFlags . _itemProduct $ i)
                                         (_productIsOrganic    . _productFlags . _itemProduct $ i)
                                         (_productIsAddedSugar . _productFlags . _itemProduct $ i)
                                         (_productIsVegan      . _productFlags . _itemProduct $ i)
                                         (groupId)
                                         (orderId)
                                         (householdId)
                                         (itemProductCode i)  
  void $ executeMany conn [sql|
    insert into v2.order_item 
      ( quantity
      , product_name
      , product_price
      , product_vat_rate
      , product_vat_rate_multiplier
      , product_is_biodynamic
      , product_is_fair_trade
      , product_is_gluten_free
      , product_is_organic
      , product_is_added_sugar
      , product_is_vegan
      , order_group_id
      , order_id
      , household_id
      , product_code
      )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] rows

updateOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
updateOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, status), i) -> 
                         UpdateOrderItem (_itemQuantity i)
                                         (_productName . _productInfo . _itemProduct $ i)
                                         (_moneyExcVat . _priceAmount . itemProductPrice $ i)
                                         (_vatRateType . _priceVatRate . itemProductPrice $ i)
                                         ((realToFrac $ _vatRateMultiplier . _priceVatRate . itemProductPrice $ i) :: Double)
                                         (_productIsBiodynamic . _productFlags . _itemProduct $ i)
                                         (_productIsFairTrade  . _productFlags . _itemProduct $ i)
                                         (_productIsGlutenFree . _productFlags . _itemProduct $ i)
                                         (_productIsOrganic    . _productFlags . _itemProduct $ i)
                                         (_productIsAddedSugar . _productFlags . _itemProduct $ i)
                                         (_productIsVegan      . _productFlags . _itemProduct $ i)
                                         (groupId)
                                         (orderId)
                                         (householdId)
                                         (itemProductCode i)
  void $ executeMany conn [sql|
    update v2.order_item
    set quantity = ?
      , product_name = ?
      , product_price = ?
      , product_vat_rate = ?
      , product_vat_rate_multiplier = ?
      , product_is_biodynamic = ?
      , product_is_fair_trade = ?
      , product_is_gluten_free = ?
      , product_is_organic = ?
      , product_is_added_sugar = ?
      , product_is_vegan = ?
    where order_group_id = ? and order_id = ? and household_id = ? and product_code = ?
  |] rows

deleteOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
deleteOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, _), i) -> (groupId, orderId, householdId, itemProductCode i)
  void $ executeMany conn [sql|
    delete from v2.order_item
    where order_group_id = ? and order_id = ? and household_id = ? and product_code = ?
  |] rows

insertOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductCode), OrderItemAdjustment)] -> IO ()
insertOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productCode), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    insert into v2.order_item_adjustment 
    ( new_vat_rate
    , new_price
    , new_quantity
    , is_discontinued
    , date
    , order_group_id
    , order_id
    , household_id
    , product_code
    )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] rows

updateOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductCode), OrderItemAdjustment)] -> IO ()
updateOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productCode), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    update v2.order_item_adjustment
    set new_vat_rate = ?
      , new_price = ?
      , new_quantity = ?
      , is_discontinued = ?
      , date = ?
    where order_group_id = ? and order_id = ? and household_id = ? and product_code = ?
  |] rows

deleteOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductCode), OrderItemAdjustment)] -> IO ()
deleteOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productCode), _) -> (groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    delete from v2.order_item_adjustment
    where order_group_id = ? and order_id = ? and household_id = ? and product_code = ?
  |] rows

toHousehold :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> [Payment] -> (HouseholdRow) -> Household
toHousehold rHouseholdOrders rOrderItems rPayments h = 
  Household householdInfo
            householdContact
            householdOrders
            rPayments
  where
  householdInfo = householdRow_householdInfo h
  householdContact = householdRow_contact h
  householdOrders = map (toHouseholdOrder rOrderItems)
                  . filter (( == _householdId householdInfo) . _householdId . householdOrderRow_householdInfo)
                  $ rHouseholdOrders      

toOrder :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> OrderRow -> Order
toOrder rHouseholdOrders rOrderItems o = 
  Order orderInfo
        (orderRow_statusFlags o)
        householdOrders
  where
  orderInfo = orderRow_orderInfo o
  householdOrders = map (toHouseholdOrder rOrderItems)
                  . filter ((== _orderId orderInfo) . _orderId . householdOrderRow_orderInfo)
                  $ rHouseholdOrders

toHouseholdOrder :: [(OrderId, HouseholdId) :. OrderItemRow] -> HouseholdOrderRow -> HouseholdOrder
toHouseholdOrder rOrderItems ho = 
  HouseholdOrder orderInfo
                 householdInfo
                 (householdOrderRow_statusFlags ho)
                 orderItems
  where
  orderInfo = householdOrderRow_orderInfo ho
  householdInfo = householdOrderRow_householdInfo ho
  orderItems = map toOrderItem 
             . filter (\((oId, hId) :. _) -> oId == _orderId orderInfo && hId == _householdId householdInfo)
             $ rOrderItems

toOrderItem :: ((OrderId, HouseholdId) :. OrderItemRow) -> OrderItem
toOrderItem (_ :. i) = OrderItem (orderItemRow_product i)
                                 (orderItemRow_quantity i)
                                 (orderItemRow_adjustment i)

instance FromField OrderGroupId where
  fromField f char = OrderGroupId <$> fromField f char

instance ToField OrderGroupId where
  toField = toField . fromOrderGroupId

data HouseholdRow = HouseholdRow
  { householdRow_householdInfo :: HouseholdInfo
  , householdRow_contact :: Contact
  }

instance FromRow HouseholdRow where
  fromRow = HouseholdRow <$> householdInfoField <*> contactField

householdInfoField :: RowParser HouseholdInfo
householdInfoField = HouseholdInfo <$> field <*> field

contactField :: RowParser Contact
contactField = Contact <$> field <*> field <*> field

instance FromField HouseholdId where
  fromField f char = HouseholdId <$> fromField f char

instance ToField HouseholdId where
  toField = toField . fromHouseholdId

data OrderRow = OrderRow
  { orderRow_orderInfo :: OrderInfo
  , orderRow_statusFlags :: OrderStatusFlags 
  }

instance FromRow OrderRow where
  fromRow = OrderRow <$> orderInfoField <*> orderStatusFlagsField

orderInfoField :: RowParser OrderInfo
orderInfoField = do
    orderId  <- field
    orderGroupId  <- field
    created  <- field
    createdById <- field
    createdByName  <- field
    return $ OrderInfo orderId orderGroupId created (createdBy createdById createdByName)
    where
    createdBy (Just id) (Just name) = Just $ HouseholdInfo id name
    createdBy _ _                   = Nothing

orderStatusFlagsField :: RowParser OrderStatusFlags
orderStatusFlagsField = OrderStatusFlags <$> field <*> field

instance FromField OrderId where
  fromField f char = OrderId <$> fromField f char

instance ToField OrderId where
  toField = toField . fromOrderId

data HouseholdOrderRow = HouseholdOrderRow
  { householdOrderRow_orderInfo :: OrderInfo
  , householdOrderRow_householdInfo :: HouseholdInfo
  , householdOrderRow_statusFlags :: HouseholdOrderStatusFlags
  }

instance FromRow HouseholdOrderRow where
  fromRow = HouseholdOrderRow <$> orderInfoField <*> householdInfoField <*> householdOrderStatusFlagsField

householdOrderStatusFlagsField :: RowParser HouseholdOrderStatusFlags
householdOrderStatusFlagsField = HouseholdOrderStatusFlags <$> field <*> field <*> field

data OrderItemRow = OrderItemRow 
  { orderItemRow_product :: Product
  , orderItemRow_quantity :: Int
  , orderItemRow_adjustment :: Maybe OrderItemAdjustment
  }

instance FromRow OrderItemRow where
  fromRow = OrderItemRow <$> productField <*> field <*> maybeOrderItemAdjustmentField

instance FromRow Product where
  fromRow = Product <$> productInfoField <*> productFlagsField

productField :: RowParser Product
productField = Product <$> productInfoField <*> productFlagsField

productInfoField :: RowParser ProductInfo
productInfoField = ProductInfo <$> field <*> field <*> priceField

instance FromField ProductId where
  fromField f char = ProductId <$> fromField f char

instance ToField ProductId where
  toField = toField . fromProductId

instance FromField ProductCode where
  fromField f char = ProductCode <$> fromField f char

instance ToField ProductCode where
  toField = toField . fromProductCode

productFlagsField :: RowParser ProductFlags
productFlagsField = ProductFlags <$> field <*> field <*> field <*> field <*> field <*> field

priceField :: RowParser Price
priceField = atVatRate <$> vatRateField <*> field

instance ToField Price where
  toField p = Many [ toField . _moneyExcVat . _priceAmount $ p
                   , toField . _vatRateType . _priceVatRate $ p
                   ]

vatRateField :: RowParser VatRate
vatRateField = VatRate <$> field <*> field

maybeOrderItemAdjustmentField :: RowParser (Maybe OrderItemAdjustment)
maybeOrderItemAdjustmentField = do
  newPrice <- maybePriceField
  newQuantity <- field
  isDiscontinued <- field
  date <- field
  case (newPrice, newQuantity, isDiscontinued, date) of
    (Just p, Just q, Just disc, Just date) -> return $ Just $ OrderItemAdjustment p q disc date
    _ -> return Nothing

maybePriceField :: RowParser (Maybe Price)
maybePriceField = do
  vatRate <- maybeVatRateField
  amount <- field
  case (vatRate, amount) of
    (Just vr, Just a) -> return $ Just $ atVatRate vr a
    _ -> return Nothing

maybeVatRateField :: RowParser (Maybe VatRate)
maybeVatRateField = do
  vatRateType <- field
  vatRateMultiplier <- field
  case (vatRateType, vatRateMultiplier) of
    (Just t, Just m) -> return $ Just $ VatRate t m
    _ -> return Nothing

instance FromRow Payment where
  fromRow = Payment <$> field <*> field <*> field <*> field

instance FromField PaymentId where
  fromField f char = PaymentId <$> fromField f char

instance ToField PaymentId where
  toField = toField . fromPaymentId

instance FromField VatRateType where
  fromField f char = do
    c <- fromField f char
    case c of
      Just 'Z' -> return Zero
      Just 'S' -> return Standard
      Just 'R' -> return Reduced
      _ -> mzero

instance ToField VatRateType where
  toField Zero = toDatabaseChar 'Z'
  toField Standard = toDatabaseChar 'S'
  toField Reduced = toDatabaseChar 'R'

instance FromRow VatRate where
  fromRow = VatRate <$> field <*> field

instance ToRow ProductCatalogueEntry where
  toRow e = [ toField $ _catalogueEntryCode e
            , toField $ _catalogueEntryCategory e
            , toField $ _catalogueEntryBrand e
            , toField $ _catalogueEntryDescription e
            , toField $ _catalogueEntryText e
            , toField $ _catalogueEntrySize e
            , toField $ _catalogueEntryPrice e
            , toField $ _catalogueEntryRrp e
            , toField $ _catalogueEntryBiodynamic e
            , toField $ _catalogueEntryFairTrade e
            , toField $ _catalogueEntryGlutenFree e
            , toField $ _catalogueEntryOrganic e
            , toField $ _catalogueEntryAddedSugar e
            , toField $ _catalogueEntryVegan e
            , toField $ _catalogueEntryUpdated e
            ]

instance FromRow ProductCatalogueEntry where
  fromRow = ProductCatalogueEntry <$> field <*> field <*> field <*> field <*> field <*> field <*> priceField <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data UpdateOrderItem = UpdateOrderItem
  { updateOrderItem_quantity :: Int
  , updateOrderItem_product_name :: String
  , updateOrderItem_product_price :: Int
  , updateOrderItem_product_vat_rate :: VatRateType
  , updateOrderItem_product_vat_rate_multiplier :: Double
  , updateOrderItem_product_is_biodynamic :: Bool
  , updateOrderItem_product_is_fair_trade :: Bool
  , updateOrderItem_product_is_gluten_free :: Bool
  , updateOrderItem_product_is_organic :: Bool
  , updateOrderItem_product_is_added_sugar :: Bool
  , updateOrderItem_product_is_vegan :: Bool
  , updateOrderItem_order_group_id :: OrderGroupId
  , updateOrderItem_order_id :: OrderId
  , updateOrderItem_household_id :: HouseholdId
  , updateOrderItem_product_code :: ProductCode
  }

instance ToRow UpdateOrderItem where
  toRow i = [ toField $ updateOrderItem_quantity i
            , toField $ updateOrderItem_product_name i
            , toField $ updateOrderItem_product_price i
            , toField $ updateOrderItem_product_vat_rate i
            , toField $ updateOrderItem_product_vat_rate_multiplier i
            , toField $ updateOrderItem_product_is_biodynamic i
            , toField $ updateOrderItem_product_is_fair_trade i
            , toField $ updateOrderItem_product_is_gluten_free i
            , toField $ updateOrderItem_product_is_organic i
            , toField $ updateOrderItem_product_is_added_sugar i
            , toField $ updateOrderItem_product_is_vegan i
            , toField $ updateOrderItem_order_group_id i
            , toField $ updateOrderItem_order_id i
            , toField $ updateOrderItem_household_id i
            , toField $ updateOrderItem_product_code i
            ]

data WhereParam = ForOrderGroup OrderGroupId
                | ForOrder OrderId
                | ForHousehold HouseholdId
                | ForProductId ProductId
                | ForProductCode ProductCode
                | ForPayment PaymentId
                | OrderIsCurrent
                | OrderIsPast

instance ToField WhereParam where
  toField (ForOrderGroup a) = toField a
  toField (ForOrder a) = toField a
  toField (ForHousehold a) = toField a
  toField (ForProductId a) = toField a
  toField (ForProductCode a) = toField a
  toField (ForPayment a) = toField a
  -- Never used but need to handle the case
  toField _ = Plain "null"

toWhereClause :: [WhereParam] -> (WhereParam -> Maybe Query) -> (Query, [WhereParam])
toWhereClause params fn = foldl' fn' ([sql| |], []) params
  where
    fn' (q, p) wp = case fn wp of
                     Just wq -> (q <> " and (" <> wq <> ")", p <> [wp])
                     _ -> (q, p)

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

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