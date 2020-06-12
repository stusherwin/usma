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
import Control.Monad (mzero, forM_, void, when, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (find, foldl', deleteFirstsBy, intersectBy)
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

createOrder :: Repository -> OrderGroupId -> OrderSpec -> IO OrderId
createOrder repo groupId spec = do
  let conn = connection repo

  [Only id] <- insertOrder conn groupId spec
  return id

createProduct :: Repository -> String -> IO (Maybe Product)
createProduct repo code = do
  let conn = connection repo

  insertProduct conn code
  listToMaybe <$> selectProducts conn [ForProduct code]

createHouseholdOrder :: Repository -> OrderGroupId -> OrderId -> HouseholdId -> UTCTime -> IO (Maybe HouseholdOrder)
createHouseholdOrder repo groupId orderId householdId date = do
  let conn = connection repo

  insertHouseholdOrder conn groupId orderId householdId date

  let params = [ForOrderGroup groupId, OrderIsCurrent, ForOrder orderId, ForHousehold householdId]
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn params
    rOrderItems <- selectOrderItemRows conn params
    return (rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

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

setHouseholdOrders :: Repository -> ([HouseholdOrder], [HouseholdOrder]) -> IO ()
setHouseholdOrders repo orders = do
    let conn = connection repo

    -- TODO?
    -- let addedOrders    = addedBy   orderKey orders
    -- let removedOrders  = removedBy orderKey orders
    updateHouseholdOrders conn $ updatedByComparing orderKey _householdOrderStatusFlags orders

    let items = join (***) (concat . map keyedItems) $ orders
    let adjustments = join (***) (catMaybes . map keyedAdjustment) $ items

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
    itemKey ((groupId, orderId, householdId, status), i) = (groupId, orderId, householdId, status, itemProductId i)
    keyedAdjustment i = (itemKey i, ) <$> (_itemAdjustment . snd) i

setProductCatalogue :: Repository -> UTCTime -> ProductCatalogue -> IO [Product]
setProductCatalogue repo date catalogue = do
  let conn = connection repo

  truncateCatalogue conn
  insertCatalogue conn catalogue
  updateProducts conn date
  selectProducts conn []

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
      _ -> Nothing

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
      _ -> Nothing

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
           , o.is_placed
           , ho.is_complete
           , ho.updated
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

selectOrderItemRows :: Connection -> [WhereParam] -> IO [(OrderId, HouseholdId) :. OrderItemRow]
selectOrderItemRows conn whereParams = 
    query conn ([sql|
      select hoi.order_id
           , hoi.household_id
           , hoi.product_id
           , coalesce(hoi.product_code, p.code)
           , coalesce(hoi.product_name, p.name)
           , coalesce(hoi.product_vat_rate, p.vat_rate)
           , coalesce(hoi.product_vat_rate_multiplier, v.multiplier)
           , coalesce(hoi.product_price, p.price)
           , coalesce(hoi.product_is_discontinued, p.is_discontinued)
           , coalesce(hoi.product_updated, p.updated)
           , coalesce(hoi.product_biodynamic, p.biodynamic)
           , coalesce(hoi.product_fair_trade, p.fair_trade)
           , coalesce(hoi.product_gluten_free, p.gluten_free)
           , coalesce(hoi.product_organic, p.organic)
           , coalesce(hoi.product_added_sugar, p.added_sugar)
           , coalesce(hoi.product_vegan, p.vegan)
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
        on p.id = hoi.product_id
      left join v2.vat_rate v
        on v.code = p.vat_rate
      left join v2.order_item_adjustment adj
        on adj.order_id = hoi.order_id and adj.household_id = hoi.household_id and adj.product_id = hoi.product_id
      left join v2.vat_rate adjv
        on adjv.code = adj.new_vat_rate
      where 1 = 1 |] <> whereClause <> [sql|
      order by hoi.ix
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      (ForHousehold _)   -> Just [sql| ho.household_id = ? |]

selectProducts :: Connection -> [WhereParam] -> IO [Product]
selectProducts conn whereParams = 
    query conn ([sql|
      select p.id
           , p.code
           , p.name
           , p.vat_rate
           , v.multiplier
           , p.price
           , p.is_discontinued
           , p.updated
           , p.biodynamic
           , p.fair_trade
           , p.gluten_free
           , p.organic
           , p.added_sugar
           , p.vegan
      from v2.product p 
      where 1 = 1 |] <> whereClause <> [sql|
      order by p.code
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForProduct _) -> Just [sql| p.code = ? |]
      _ -> Nothing

truncateCatalogue :: Connection -> IO ()
truncateCatalogue conn =   
  void $ execute_ conn [sql|
    truncate table catalogue_entry
  |]

insertCatalogue :: Connection -> ProductCatalogue -> IO ()
insertCatalogue conn catalogue = 
  void $ executeMany conn [sql|
    insert into catalogue_entry (code, category, brand, "description", "text", size, price, vat_rate, rrp, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan, updated)
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] catalogue

updateProducts :: Connection -> UTCTime -> IO ()
updateProducts conn date = 
  void $ execute conn [sql|
    update product
    set "name" = case when ce.code is null then p."name" else concat_ws(' ', nullif(btrim(ce.brand), '')
                                                                           , nullif(btrim(ce."description"), '')
                                                                           , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                                                                           , nullif(btrim(ce."text"), '')) end
      , price = coalesce(ce.price, p.price)
      , vat_rate = coalesce(ce.vat_rate, p.vat_rate)
      , biodynamic = coalesce(ce.biodynamic, p.biodynamic)
      , fair_trade = coalesce(ce.fair_trade, p.fair_trade)
      , gluten_free = coalesce(ce.gluten_free, p.gluten_free)
      , organic = coalesce(ce.organic, p.organic)
      , added_sugar = coalesce(ce.added_sugar, p.added_sugar)
      , vegan = coalesce(ce.vegan, p.vegan)
      , discontinued = (ce.code is null)
      , updated = case when ce.price <> p.price or ce.code is null then ? else p.updated end
    from product p
    left join catalogue_entry ce on p.code = ce.code
    where p.id = product.id
  |] (Only date)
  
insertOrder :: Connection -> OrderGroupId -> OrderSpec -> IO [Only OrderId]
insertOrder conn groupId spec = 
  query conn [sql|
    insert into "order" (order_group_id, created_date, created_by_id) 
    values (?, ?, ?) 
    returning id
  |] (groupId, _orderSpecCreated spec, _orderSpecCreatedByHouseholdId spec)

insertHouseholdOrder :: Connection -> OrderGroupId -> OrderId -> HouseholdId -> UTCTime -> IO ()
insertHouseholdOrder conn groupId orderId householdId date =
  void $ execute conn [sql|
    insert into household_order (order_group_id, order_id, household_id, updated, is_complete, is_abandoned) 
    values (?, ?, ?, ?, false, false)
    on conflict (order_group_id, order_id, household_id) do nothing
  |] (groupId, orderId, householdId, date)

insertProduct :: Connection -> String -> IO ()
insertProduct conn code =
  void $ execute conn [sql|
    insert into product ("code", "name", price, vat_rate, discontinued, updated, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan) 
    select ce.code
         , concat_ws(' ', nullif(btrim(ce.brand), '')
                        , nullif(btrim(ce."description"), '')
                        , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                        , nullif(btrim(ce."text"), ''))
         , ce.price
         , ce.vat_rate
         , false
         , ce.updated
         , ce.biodynamic
         , ce.fair_trade 
         , ce.gluten_free 
         , ce.organic
         , ce.added_sugar
         , ce.vegan                    
    from catalogue_entry ce
    where ce.code = ?
    on conflict ("code") do nothing
  |] (Only code)

updateOrders :: Connection -> [Order] -> IO ()
updateOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _orderInfo $ o
                                  orderId = _orderId . _orderInfo $ o
                                  abandoned = orderIsAbandoned o
                                  complete = orderIsComplete o
                              in (abandoned, complete, groupId, orderId)
  void $ executeMany conn [sql|
    update "order"
    set is_abandoned = ? 
      , is_complete = ?
    where order_group_id = ? and order_id = ?
  |] rows

updateHouseholdOrders :: Connection -> [HouseholdOrder] -> IO ()
updateHouseholdOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                                  orderId = _orderId . _householdOrderOrderInfo $ o
                                  householdId = _householdId . _householdOrderHouseholdInfo $ o
                                  abandoned = householdOrderIsAbandoned o
                                  complete = householdOrderIsComplete o
                              in (abandoned, complete, groupId, orderId, householdId)
  void $ executeMany conn [sql|
    update household_order 
    set is_abandoned = ? 
      , is_complete = ?
    where order_group_id = ? and order_id = ? and household_id = ?
  |] rows

insertOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
insertOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, _), i) -> (_itemQuantity i, groupId, orderId, householdId, itemProductId i)
  void $ executeMany conn [sql|
    insert into order_item 
      ( quantity
      , order_group_id
      , order_id
      , household_id
      , product_id
      )
    values (?, ?, ?, ?, ?)
  |] rows

updateOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
updateOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, status), i) ->
                         UpdateOrderItemRow (_itemQuantity i)
                                            (justWhen (_itemProduct i) (isPastStatus status))
                                            groupId
                                            orderId
                                            householdId
                                            (itemProductId i)
  void $ executeMany conn [sql|
    update order_item
    set quantity = ?
      , product_code = ?
      , product_name = ?
      , product_vat_rate = ?
      , product_vat_rate_multiplier = ?
      , product_price = ?
      , product_is_discontinued = ?
      , product_updated = ?
      , product_is_biodynamic = ?
      , product_is_fair_trade = ?
      , product_is_gluten_free = ?
      , product_is_organic = ?
      , product_is_added_sugar = ?
      , product_is_vegan = ?
    where order_group_id = ? and order_id = ? and household_id = ? and product_id = ?
  |] rows

deleteOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus), OrderItem)] -> IO ()
deleteOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, _), i) -> (groupId, orderId, householdId, itemProductId i)
  void $ executeMany conn [sql|
    delete from order_item
    where order_group_id = ? and order_id = ? and household_id = ? and product_id = ?
  |] rows

insertOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductId), OrderItemAdjustment)] -> IO ()
insertOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productId), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productId)
  void $ executeMany conn [sql|
    insert into order_item_adjustment 
    ( new_vat_rate
    , new_price
    , new_quantity
    , is_discontinued
    , date
    , order_group_id
    , order_id
    , household_id
    , product_id
    )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] rows

updateOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductId), OrderItemAdjustment)] -> IO ()
updateOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productId), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productId)
  void $ executeMany conn [sql|
    update order_item_adjustment
    set new_vat_rate = ?
      , new_price = ?
      , new_quantity = ?
      , is_discontinued = ?
      , date = ?
    where order_group_id = ? and order_id = ? and household_id = ? and product_id = ?
  |] rows

deleteOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, HouseholdOrderStatus, ProductId), OrderItemAdjustment)] -> IO ()
deleteOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, _, productId), _) -> (groupId, orderId, householdId, productId)
  void $ executeMany conn [sql|
    delete from order_item_adjustment
    where order_group_id = ? and order_id = ? and household_id = ? and product_id = ?
  |] rows

toHousehold :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> [Payment] -> (HouseholdRow) -> Household
toHousehold rHouseholdOrders rOrderItems rPayments h = 
  Household householdInfo
            (householdRow_contact_name h)
            (householdRow_contact_email h)
            (householdRow_contact_phone h)
            householdOrders
            rPayments
  where
  householdInfo = householdRow_householdInfo h
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
  , householdRow_contact_name :: Maybe String
  , householdRow_contact_email :: Maybe String
  , householdRow_contact_phone :: Maybe String
  }

instance FromRow HouseholdRow where
  fromRow = HouseholdRow <$> householdInfoField <*> field <*> field <*> field

householdInfoField :: RowParser HouseholdInfo
householdInfoField = HouseholdInfo <$> field <*> field

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
householdOrderStatusFlagsField = HouseholdOrderStatusFlags <$> field <*> field <*> field <*> field

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
productInfoField = ProductInfo <$> field <*> field <*> field <*> priceField <*> field <*> field

instance FromField ProductId where
  fromField f char = ProductId <$> fromField f char

instance ToField ProductId where
  toField = toField . fromProductId

productFlagsField :: RowParser ProductFlags
productFlagsField = ProductFlags <$> field <*> field <*> field <*> field <*> field <*> field

priceField :: RowParser Price
priceField = atVatRate <$> vatRateField <*> field

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

instance ToRow ProductCatalogueEntry where
  toRow e = [ toField $ _catalogueEntryCode e
            , toField $ _catalogueEntryCategory e
            , toField $ _catalogueEntryBrand e
            , toField $ _catalogueEntryDescription e
            , toField $ _catalogueEntryText e
            , toField $ _catalogueEntrySize e
            , toField $ _catalogueEntryPrice e
            , toField $ _catalogueEntryVatRateType e
            , toField $ _catalogueEntryRrp e
            , toField $ _catalogueEntryBiodynamic e
            , toField $ _catalogueEntryFairTrade e
            , toField $ _catalogueEntryGlutenFree e
            , toField $ _catalogueEntryOrganic e
            , toField $ _catalogueEntryAddedSugar e
            , toField $ _catalogueEntryVegan e
            , toField $ _catalogueEntryUpdated e
            ]

data UpdateOrderItemRow = UpdateOrderItemRow 
  { updateOrderItem_quantity :: Int
  , updateOrderItem_product :: Maybe Product
  , updateOrderItem_groupId :: OrderGroupId
  , updateOrderItem_orderId :: OrderId
  , updateOrderItem_householdId :: HouseholdId
  , updateOrderItem_productId :: ProductId
  }

instance ToRow UpdateOrderItemRow where
  toRow i = [ toField $ updateOrderItem_quantity i
            , toField $ _productCode . _productInfo <$> updateOrderItem_product i
            , toField $ _productName . _productInfo <$> updateOrderItem_product i
            , toField $ _vatRateType . _priceVatRate . _productPrice . _productInfo <$> updateOrderItem_product i
            , toField $ ((realToFrac . _vatRateMultiplier . _priceVatRate . _productPrice . _productInfo <$> updateOrderItem_product i) :: Maybe Double )
            , toField $ _moneyExcVat . _priceAmount . _productPrice . _productInfo <$> updateOrderItem_product i
            , toField $ _productIsDiscontinued . _productInfo <$> updateOrderItem_product i
            , toField $ _productUpdated . _productInfo <$> updateOrderItem_product i
            , toField $ _productIsBiodynamic . _productFlags <$> updateOrderItem_product i
            , toField $ _productIsFairTrade . _productFlags <$> updateOrderItem_product i
            , toField $ _productIsGlutenFree . _productFlags <$> updateOrderItem_product i
            , toField $ _productIsOrganic . _productFlags <$> updateOrderItem_product i
            , toField $ _productIsAddedSugar . _productFlags <$> updateOrderItem_product i
            , toField $ _productIsVegan . _productFlags <$> updateOrderItem_product i
            , toField $ updateOrderItem_groupId i
            , toField $ updateOrderItem_orderId i
            , toField $ updateOrderItem_householdId i
            , toField $ updateOrderItem_productId i
            ]

data WhereParam = ForOrderGroup OrderGroupId
                | ForOrder OrderId
                | ForHousehold HouseholdId
                | ForProduct String
                | OrderIsCurrent
                | OrderIsPast

instance ToField WhereParam where
  toField (ForOrderGroup a) = toField a
  toField (ForOrder a) = toField a
  toField (ForHousehold a) = toField a
  toField (ForProduct a) = toField a
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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &

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