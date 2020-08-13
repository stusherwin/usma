{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database ( getCollectiveOrder, getHouseholdOrders, getPastCollectiveOrders, getPastHouseholdOrders, getHouseholds, getHouseholdPayments, getProductCatalogue
                , createOrder, closeOrder
                , cancelHouseholdOrder, completeHouseholdOrder, reopenHouseholdOrder
                , ensureHouseholdOrderItem, ensureAllItemsFromPastHouseholdOrder, removeHouseholdOrderItem
                , createHousehold, updateHousehold, archiveHousehold
                , createHouseholdPayment, updateHouseholdPayment, archiveHouseholdPayment
                , replaceProductCatalogue, acceptCatalogueUpdates
                , getProductCatalogueCategories, getProductCatalogueBrands, getProductImage, saveProductImage
                , getGroup
                , reconcileOrderItem, reconcileHouseholdOrderItems
                , getGroupSettings
                , getUploadedOrderFile, saveUploadedOrderFile, deleteUploadedOrderFile
                ) where

import Control.Monad (void, forM_)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Types
import DatabaseTypes  

instance FromRow GroupSettings where
  fromRow = GroupSettings <$> field

getCollectiveOrder :: ByteString -> Int -> IO (Maybe CollectiveOrder)
getCollectiveOrder connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  (rOrders, rItems) <- withTransaction conn $ do
    rOrders <- getCollectiveOrderData conn groupId
    rItems <- getCollectiveOrderItemData conn groupId
    return (rOrders, rItems)
  close conn
  return $ listToMaybe $ map (fromCollectiveOrderData rItems) rOrders

getPastCollectiveOrders :: ByteString -> Int -> IO [PastCollectiveOrder]
getPastCollectiveOrders connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  (rOrders, rItems) <- withTransaction conn $ do
    rOrders <- getPastCollectiveOrderData conn groupId
    rItems <- getPastOrderItemData conn groupId
    return (rOrders, rItems)
  close conn
  return $ map (fromPastCollectiveOrderData rItems) rOrders

getHouseholdOrders :: ByteString -> Int -> IO [HouseholdOrder]
getHouseholdOrders connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  (rOrders, rItems) <- withTransaction conn $ do
    rOrders <- getHouseholdOrderData conn groupId
    rItems <- getHouseholdOrderItemData conn groupId
    return (rOrders, rItems)
  close conn
  return $ map (fromHouseholdOrderData rItems) rOrders

getPastHouseholdOrders :: ByteString -> Int -> IO [PastHouseholdOrder]
getPastHouseholdOrders connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  (rOrders, rItems) <- withTransaction conn $ do
    rOrders <- getPastHouseholdOrderData conn groupId
    rItems <- getPastHouseholdOrderItemData conn groupId
    return (rOrders, rItems)
  close conn
  return $ map (fromPastHouseholdOrderData rItems) rOrders



getCollectiveOrderData :: Connection -> Int -> IO [CollectiveOrderData]
getCollectiveOrderData conn groupId = 
  query conn [sql|
    with orders as (
      select o.id, o.created_date, h.id as created_by_id, h.name as created_by_name
      from "order" o
      left join household h on h.id = o.created_by_id
      left join household_order ho on ho.order_id = o.id
      left join household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = ho.order_id
      where o.order_group_id = ?
      group by o.id, o.created_date, h.id, h.name
      order by o.id desc
    ),
    household_orders as (
      select ho.order_id, ho.household_id, ho.complete, 
        count(hoi.product_id) as item_count,
        max(p.updated) <= ho.updated as up_to_date,
        coalesce(sum(hoi.item_total_exc_vat), 0) as old_total_exc_vat, 
        coalesce(sum(hoi.item_total_inc_vat), 0) as old_total_inc_vat,
        coalesce(sum(case when p.discontinued then 0 
                          else p.price * hoi.quantity
                     end), 0) as total_exc_vat,
        coalesce(sum(case when p.discontinued then 0 
                          else cast(round(p.price * v.multiplier) as int) * hoi.quantity
                     end), 0) as total_inc_vat
      from household_order ho
      inner join household_order_item hoi on hoi.household_id = ho.household_id
      inner join product p on p.id = hoi.product_id
      inner join vat_rate v on v.code = p.vat_rate
      where ho.cancelled = false and ho.order_group_id = ?
      group by ho.order_id, ho.household_id, ho.complete
    )
      select o.id, o.created_date, o.created_by_id, o.created_by_name, 
      coalesce(bool_and(ho.complete or ho.item_count = 0), false) as complete,
      cast(coalesce(sum(ho.old_total_exc_vat), 0) as int) as old_total_exc_vat,
      cast(coalesce(sum(ho.old_total_inc_vat), 0) as int) as old_total_inc_vat,
      cast(coalesce(sum(ho.total_exc_vat), 0) as int) as total_exc_vat,
      cast(coalesce(sum(ho.total_inc_vat), 0) as int) as new_total_inc_vat,
      coalesce(bool_and(ho.up_to_date), true) as all_up_to_date
    from orders o
    left join household_orders ho on ho.order_id = o.id
    group by o.id, o.created_date, o.created_by_id, o.created_by_name
    order by o.id desc
  |] (groupId, groupId)

getPastCollectiveOrderData :: Connection -> Int -> IO [PastCollectiveOrderData]
getPastCollectiveOrderData conn groupId = 
  query conn [sql|
    select o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled, 
      bool_and(adj.order_id is not null) as reconciled,
      coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
      coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat,
      sum(coalesce(adj.old_item_total_exc_vat, hoi.item_total_exc_vat)) as old_total_exc_vat,
      sum(coalesce(adj.old_item_total_inc_vat, hoi.item_total_inc_vat)) as old_total_inc_vat
    from past_order o
    left join past_household_order ho on ho.order_id = o.id and ho.cancelled = false
    left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
    left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
    where o.order_group_id = ?
    group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled
    order by o.id desc
  |] (Only groupId)

getHouseholdOrderData :: Connection -> Int -> IO [HouseholdOrderData]
getHouseholdOrderData conn groupId = 
  query conn [sql|
    select o.id
         , o.created_date
         , cb.id as created_by_id
         , cb.name as created_by_name
         , h.id
         , h.name
         , ho.complete
         , ho.cancelled
         , coalesce(sum(hoi.item_total_exc_vat), 0) as old_total_exc_vat
         , coalesce(sum(hoi.item_total_inc_vat), 0) as old_total_inc_vat
         , coalesce(sum(case when p.discontinued then 0 
                             else p.price * hoi.quantity
                        end), 0) as total_exc_vat
         , coalesce(sum(case when p.discontinued then 0 
                             else cast(round(p.price * v.multiplier) as int) * hoi.quantity
                        end), 0) as total_inc_vat
         , max(p.updated) is not null and max(p.updated) > ho.updated as updated
    from household_order ho
    inner join "order" o on o.id = ho.order_id
    left join household cb on cb.id = o.created_by_id
    inner join household h on h.id = ho.household_id
    left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
    left join product p on p.id = hoi.product_id
    left join vat_rate v on v.code = p.vat_rate
    where o.order_group_id = ?
    group by o.id, o.created_date, cb.id, cb.name, h.id, h.name, ho.complete, ho.cancelled, ho.updated
    order by o.id desc, h.name asc
  |] (Only groupId)
  
getPastHouseholdOrderData :: Connection -> Int -> IO [PastHouseholdOrderData]
getPastHouseholdOrderData conn groupId =
  query conn [sql|
    select o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled, ho.household_id, ho.household_name, ho.cancelled, 
      bool_and(adj.order_id is not null) as reconciled,
      coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
      coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat,
      sum(coalesce(adj.old_item_total_exc_vat, hoi.item_total_exc_vat)) as old_total_exc_vat,
      sum(coalesce(adj.old_item_total_inc_vat, hoi.item_total_inc_vat)) as old_total_inc_vat
    from past_household_order ho
    inner join past_order o on o.id = ho.order_id
    left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
    left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
    where o.order_group_id = ?
    group by o.id, o.created_date, o.created_by_id, o.created_by_name, ho.household_id, ho.household_name, ho.cancelled
    order by o.id desc, ho.household_name asc
  |] (Only groupId)



getCollectiveOrderItemData :: Connection -> Int -> IO [CollectiveOrderItemData]
getCollectiveOrderItemData conn groupId =
  query conn [sql|
    select hoi.order_id
         , p.id
         , p.code
         , p.name
         , p.vat_rate
         , case when p.discontinued then 0
                else sum(hoi.quantity)
           end as quantity
         , p.price as product_price_exc_vat
         , cast(round(p.price * v.multiplier) as int) as product_price_inc_vat
         , case when p.discontinued then 0 
                else sum(p.price * hoi.quantity)
           end as item_total_exc_vat
         , case when p.discontinued then 0 
                else sum(cast(round(p.price * v.multiplier) as int) * hoi.quantity)
           end as item_total_inc_vat
         , p.biodynamic
         , p.fair_trade
         , p.gluten_free
         , p.organic
         , p.added_sugar
         , p.vegan
         , hoi.product_price_exc_vat as old_product_price_exc_vat         
         , hoi.product_price_inc_vat as old_product_price_inc_vat
         , sum(hoi.quantity) as old_quantity
         , sum(hoi.item_total_exc_vat) as old_item_total_exc_vat
         , sum(hoi.item_total_inc_vat) as old_item_total_inc_vat
         , p.discontinued
         
         , p.updated > max(ho.updated) as updated

    from household_order_item hoi
    inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join product p on p.id = hoi.product_id
    inner join vat_rate v on v.code = p.vat_rate
    where ho.order_group_id = ? and not ho.cancelled
    group by hoi.order_id, p.id, p.code, p.name, p.vat_rate, p.price, v.multiplier, p.biodynamic, p.fair_trade, p.gluten_free, p.organic, p.added_sugar, p.vegan, hoi.product_price_exc_vat, hoi.product_price_inc_vat
    order by p.code
  |] (Only groupId)

getPastOrderItemData :: Connection -> Int -> IO [(Int, PastOrderItemData)]
getPastOrderItemData conn groupId = do
  items <- query conn [sql|
    select hoi.order_id, 
           hoi.product_id, 
           hoi.product_code, 
           hoi.product_name, 
           hoi.product_price_exc_vat, 
           hoi.product_price_inc_vat,
           hoi.product_vat_rate, 
           sum(hoi.quantity) as quantity, 
           sum(hoi.item_total_exc_vat) as item_total_exc_vat, 
           sum(hoi.item_total_inc_vat) as item_total_inc_vat,
           hoi.product_biodynamic,
           hoi.product_fair_trade,
           hoi.product_gluten_free,
           hoi.product_organic,
           hoi.product_added_sugar,
           hoi.product_vegan,
           case 
             when bool_or(adj.old_product_price_exc_vat is not null) then max(coalesce(adj.old_product_price_exc_vat, hoi.product_price_exc_vat)) 
             else null
           end as old_product_price_exc_vat,
           case 
             when bool_or(adj.old_product_price_inc_vat is not null) then max(coalesce(adj.old_product_price_inc_vat, hoi.product_price_inc_vat)) 
           end as old_product_price_inc_vat,
           case 
             when bool_or(adj.old_quantity is not null) then sum(coalesce(adj.old_quantity, hoi.quantity)) 
           end as old_quantity,
           case 
             when bool_or(adj.old_item_total_exc_vat is not null) then sum(coalesce(adj.old_item_total_exc_vat, hoi.item_total_exc_vat)) 
           end as old_item_total_exc_vat,
           case 
             when bool_or(adj.old_item_total_inc_vat is not null) then sum(coalesce(adj.old_item_total_inc_vat, hoi.item_total_inc_vat))
           end as old_item_total_inc_vat
    from past_household_order_item hoi
    inner join past_household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
    where hoi.order_group_id = ? and not ho.cancelled
    group by hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, hoi.product_biodynamic, hoi.product_fair_trade, hoi.product_gluten_free, hoi.product_organic, hoi.product_added_sugar, hoi.product_vegan
    order by hoi.product_code
  |] (Only groupId)
  return $ map (\((Only id) :. d) -> (id, d)) items

getHouseholdOrderItemData :: Connection -> Int -> IO [HouseholdOrderItemData]
getHouseholdOrderItemData conn groupId = 
  query conn [sql|
    select hoi.order_id
         , hoi.household_id
         , p.id
         , p.code
         , p.name
         , p.price as product_price_exc_vat
         , cast(round(p.price * v.multiplier) as int) as product_price_inc_vat
         , p.vat_rate
         , case when p.discontinued then 0
                else hoi.quantity
           end as quantity
         , case when p.discontinued then 0 
                else p.price * hoi.quantity
           end as item_total_exc_vat
         , case when p.discontinued then 0 
                else cast(round(p.price * v.multiplier) as int) * hoi.quantity
           end as item_total_inc_vat
         , p.biodynamic
         , p.fair_trade
         , p.gluten_free
         , p.organic
         , p.added_sugar
         , p.vegan
         , hoi.product_price_exc_vat as old_product_price_exc_vat         
         , hoi.product_price_inc_vat as old_product_price_inc_vat
         , hoi.quantity as old_quantity
         , hoi.item_total_exc_vat as old_item_total_exc_vat
         , hoi.item_total_inc_vat as old_item_total_inc_vat
         , p.discontinued
         
         , p.updated > ho.updated as updated
    from household_order_item hoi
    inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join product p on p.id = hoi.product_id
    inner join vat_rate v on v.code = p.vat_rate
    where ho.order_group_id = ?
    order by p.code
  |] (Only groupId)

getPastHouseholdOrderItemData :: Connection -> Int -> IO [((Int, Int), PastOrderItemData)]
getPastHouseholdOrderItemData conn groupId = do
  items <- query conn [sql|
    select hoi.order_id, 
           hoi.household_id, 
           hoi.product_id, 
           hoi.product_code, 
           hoi.product_name, 
           hoi.product_price_exc_vat, 
           hoi.product_price_inc_vat, 
           hoi.product_vat_rate,
           hoi.quantity,
           hoi.item_total_exc_vat,
           hoi.item_total_inc_vat,
           hoi.product_biodynamic,
           hoi.product_fair_trade,
           hoi.product_gluten_free,
           hoi.product_organic,
           hoi.product_added_sugar,
           hoi.product_vegan,
           adj.old_product_price_exc_vat,
           adj.old_product_price_inc_vat,
           adj.old_quantity,
           adj.old_item_total_exc_vat,
           adj.old_item_total_inc_vat
    from past_household_order_item hoi
    left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
    where hoi.order_group_id = ?
    order by hoi.product_code
  |] (Only groupId)
  return $ map (\(id :. d) -> (id, d)) items



getHouseholds :: ByteString -> Int -> IO [Household]
getHouseholds connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  rHouseholds <- query conn [sql|
      with household_total_orders as (
          select id, cast(sum(total) as int) as total
          from (
            (select h.id
                  , coalesce(sum(case when p.discontinued then 0 
                                 else cast(round(p.price * v.multiplier) as int) * hoi.quantity
                            end), 0) as total
            from household h
            left join household_order ho on ho.household_id = h.id and ho.cancelled = false
            left join household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = ho.order_id
            left join product p on p.id = hoi.product_id
            left join vat_rate v on v.code = p.vat_rate
            group by h.id)
            union all
            (select h.id, coalesce(sum(hoi.item_total_inc_vat), 0) as total
            from household h
            left join past_household_order ho on ho.household_id = h.id and ho.cancelled = false
            left join past_order o on ho.order_id = o.id and o.cancelled = false
            left join past_household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = o.id
            group by h.id)
          ) as h
          group by id
        ),
        household_total_payments as (
          select h.id, coalesce(sum(hp.amount), 0) as total
          from household h
          left join household_payment hp on hp.household_id = h.id and hp.archived = false
          group by h.id
        )
    select h.id, h.name, h.contact_name, h.contact_email, h.contact_phone, hto.total as total_orders, htp.total as total_payments, htp.total - hto.total as balance
    from household h
    inner join household_total_orders hto on hto.id = h.id
    left join household_total_payments htp on htp.id = h.id
    where h.archived = false and h.order_group_id = ?
    order by h.name asc
  |] (Only groupId)
  close conn
  return $ (rHouseholds :: [(Int, String, Maybe String, Maybe String, Maybe String, Int, Int, Int)]) <&> \(id, name, contactName, contactEmail, contactPhone, totalOrders, totalPayments, balance) -> Household id name contactName contactEmail contactPhone totalOrders totalPayments balance

getHouseholdPayments :: ByteString -> Int -> IO [HouseholdPayment]
getHouseholdPayments connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  rPayments <- query conn [sql|
    select p.id, p.household_id, p."date", p.amount
    from household_payment p
    where p.archived = false and p.order_group_id = ?
    order by p.id asc
  |] (Only groupId)
  close conn
  return $ (rPayments :: [(Int, Int, UTCTime, Int)]) <&> \(id, householdId, date, amount) -> HouseholdPayment id householdId date amount

createOrder :: ByteString -> Int -> UTCTime -> Maybe Int -> IO Int
createOrder connectionString groupId date householdId = do
  conn <- connectPostgreSQL connectionString
  id <- withTransaction conn $ do
    [Only id] <- query conn [sql|
      insert into "order" (order_group_id, created_date, created_by_id) values (?, ?, ?) returning id
    |] (groupId, date, householdId)
    -- case householdId of
    --   (Just householdId) -> do
    --     void $ execute conn [sql|
    --       insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
    --     |] (groupId, id, householdId, date)
    --   _ -> return ()
    return id
  close conn
  return id

closeOrder :: ByteString -> Int -> Bool -> Int -> IO ()
closeOrder connectionString groupId cancelled orderId = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    void $ execute conn [sql|
      insert into past_order (order_group_id, id, created_date, created_by_id, created_by_name, cancelled)
      select o.order_group_id, o.id, o.created_date, h.id, h.name, ?
      from "order" o
      left join household h on h.id = o.created_by_id
      where o.id = ? and o.order_group_id = ?
    |] (cancelled, orderId, groupId)
    void $ execute conn [sql|
      insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled)
      select ho.order_group_id, ho.order_id, h.id, h.name, not ho.complete
      from household_order ho
      inner join household h on h.id = ho.household_id
      where ho.order_id = ? and ho.order_group_id = ?
    |] (orderId, groupId)
    void $ execute conn [sql|
      insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat, product_biodynamic, product_fair_trade, product_gluten_free, product_organic, product_added_sugar, product_vegan)
      select hoi.order_group_id, hoi.order_id, hoi.household_id, hoi.product_id, p.code, p.name, hoi.product_price_exc_Vat, hoi.product_price_inc_vat, p.vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat, p.biodynamic, p.fair_trade, p.gluten_free, p.organic, p.added_sugar, p.vegan
      from household_order_item hoi
      inner join product p on hoi.product_id = p.id
      inner join vat_rate v on v.code = p.vat_rate
      where hoi.order_id = ? and hoi.order_group_id = ?
    |] (orderId, groupId)
    void $ execute conn [sql|
      delete from household_order_item where order_id = ? and order_group_id = ?
    |] (orderId, groupId)
    void $ execute conn [sql|
      delete from household_order where order_id = ? and order_group_id = ?
    |] (orderId, groupId)
    void $ execute conn [sql|
      delete from "order" where id = ? and order_group_id = ?
    |] (orderId, groupId)
  close conn

cancelHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
cancelHouseholdOrder connectionString groupId orderId householdId = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    void $ execute conn [sql|
      update household_order set cancelled = true where order_id = ? and household_id = ? and order_group_id = ?
    |] (orderId, householdId, groupId)
  close conn

completeHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
completeHouseholdOrder connectionString groupId orderId householdId = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household_order set complete = true where order_id = ? and household_id = ? and order_group_id = ?
  |] (orderId, householdId, groupId)
  close conn

reopenHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
reopenHouseholdOrder connectionString groupId orderId householdId = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household_order set cancelled = false, complete = false where order_id = ? and household_id = ? and order_group_id = ?
  |] (orderId, householdId, groupId)
  close conn

ensureHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> String -> UTCTime -> HouseholdOrderItemDetails -> IO ()
ensureHouseholdOrderItem connectionString groupId orderId householdId productCode date details = do
  let quantity = hoidetQuantity details
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    maybeProductId <- do
      ids <- query conn [sql|
        select id from product where code = ?
      |] (Only productCode)
      case ids of
        ((Only id):_) -> return id
        _ -> do
          maybeIds <- query conn [sql|
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
            returning id
          |] (Only productCode)
          case (maybeIds :: [Only Int]) of
            ((Only id):_) -> return $ Just id
            _ -> return Nothing
    case maybeProductId of
      Nothing -> return ()
      (Just productId) -> do
        householdOrderExists <- query conn [sql|
          select household_id from household_order where order_id = ? and household_id = ? and order_group_id = ?
        |] (orderId, householdId, groupId)
        case (householdOrderExists :: [Only Int]) of 
          ((Only _):_) -> return ()
          _ -> do
            void $ execute conn [sql|
              insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
            |] (groupId, orderId, householdId, date)
        itemExists <- query conn [sql|
          select quantity from household_order_item where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
        |] (orderId, householdId, productId, groupId)
        case (itemExists :: [Only Int]) of 
          ((Only existingQuantity):_) -> do
            void $ execute conn [sql|
              update household_order_item hoi set 
                quantity = ?, 
                item_total_exc_vat = hoi.product_price_exc_vat * ?,
                item_total_inc_vat = hoi.product_price_inc_vat * ?
              where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
            |] (maybe existingQuantity Prelude.id quantity, maybe existingQuantity Prelude.id quantity, maybe existingQuantity Prelude.id quantity, orderId, householdId, productId, groupId)  
          _ -> do
            void $ execute conn [sql|
              insert into household_order_item (order_group_id, order_id, household_id, product_id, product_price_exc_vat, product_price_inc_vat, quantity, item_total_exc_vat, item_total_inc_vat)
              select ?, ?, ?, p.id, p.price as price_exc_Vat, cast(round(p.price * v.multiplier) as int) as price_inc_vat, ?, p.price * ? as item_total_exc_vat, cast(round(p.price * v.multiplier) as int) * ? as item_total_inc_vat
              from product p
              inner join vat_rate v on v.code = p.vat_rate
              where p.id = ?
            |] (groupId, orderId, householdId, maybe 1 Prelude.id quantity, maybe 1 Prelude.id quantity, maybe 1 Prelude.id quantity, productId)
  close conn

ensureAllItemsFromPastHouseholdOrder :: ByteString -> Int -> Int -> Int -> Int -> UTCTime -> IO ()
ensureAllItemsFromPastHouseholdOrder connectionString groupId orderId householdId pastOrderId date = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
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
      inner join past_household_order_item phoi on ce.code = phoi.product_code
      where phoi.order_group_id = ? and phoi.order_id = ? and phoi.household_id = ? and phoi.product_code not in (select code from product)
    |] (groupId, pastOrderId, householdId)
    householdOrderExists <- query conn [sql|
      select household_id from household_order where order_id = ? and household_id = ? and order_group_id = ?
    |] (orderId, householdId, groupId)
    case (householdOrderExists :: [Only Int]) of 
      ((Only _):_) -> return ()
      _ -> do
        void $ execute conn [sql|
          insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
        |] (groupId, orderId, householdId, date)
    void $ execute conn [sql|
      insert into household_order_item (order_group_id, order_id, household_id, product_id, product_price_exc_vat, product_price_inc_vat, quantity, item_total_exc_vat, item_total_inc_vat)
      select ?, ?, ?, p.id, p.price as price_exc_Vat, cast(round(p.price * v.multiplier) as int) as price_inc_vat, 1, p.price as item_total_exc_vat, cast(round(p.price * v.multiplier) as int) as item_total_inc_vat
      from past_household_order_item phoi
      inner join product p on p.code = phoi.product_code
      inner join vat_rate v on v.code = p.vat_rate
      where phoi.order_group_id = ? and phoi.order_id = ? and phoi.household_id = ? and p.id not in (select product_id from household_order_item where order_group_id = ? and order_id = ? and household_id = ?)
    |] (groupId, orderId, householdId, groupId, pastOrderId, householdId, groupId, orderId, householdId)
  close conn

removeHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> Int -> IO ()
removeHouseholdOrderItem connectionString groupId orderId householdId productId = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    delete from household_order_item where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
  |] (orderId, householdId, productId, groupId)
  close conn

createHousehold :: ByteString -> Int -> HouseholdDetails -> IO Int
createHousehold connectionString groupId details = do
  conn <- connectPostgreSQL connectionString
  [Only id] <- query conn [sql|
    insert into household (order_group_id, name, contact_name, contact_email, contact_phone, archived) values (?, ?, ?, ?, ?, false) returning id
  |] (groupId, hdetName details, hdetContactName details, hdetContactEmail details, hdetContactPhone details)
  close conn
  return id

updateHousehold :: ByteString -> Int -> Int -> HouseholdDetails -> IO ()
updateHousehold connectionString groupId householdId details = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household set 
      name = ?, contact_name = ?, contact_email = ?, contact_phone = ? 
    where id = ? and order_group_id = ?
  |] (hdetName details, hdetContactName details, hdetContactEmail details, hdetContactPhone details, householdId, groupId)
  close conn

archiveHousehold :: ByteString -> Int -> Int -> IO ()
archiveHousehold connectionString groupId householdId = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household set archived = true where id = ? and order_group_id = ?
  |] (householdId, groupId)
  close conn

createHouseholdPayment :: ByteString -> Int -> Int -> HouseholdPaymentDetails -> IO Int
createHouseholdPayment connectionString groupId householdId details = do
  let date = UTCTime (hpdetDate details) (secondsToDiffTime 0)
  let amount = hpdetAmount details
  conn <- connectPostgreSQL connectionString
  [Only id] <- query conn [sql|
    insert into household_payment (order_group_id, household_id, "date", amount, archived) values (?, ?, ?, ?, false) returning id
  |] (groupId, householdId, date, amount)
  close conn
  return id

updateHouseholdPayment :: ByteString -> Int -> Int -> HouseholdPaymentDetails -> IO ()
updateHouseholdPayment connectionString groupId paymentId details = do
  let date = UTCTime (hpdetDate details) (secondsToDiffTime 0)
  let amount = hpdetAmount details
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household_payment set "date" = ?, amount = ? where id = ? and order_group_id = ?
  |] (date, amount, paymentId, groupId)
  close conn

archiveHouseholdPayment :: ByteString -> Int -> Int -> IO ()
archiveHouseholdPayment connectionString groupId id = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    update household_payment set archived = true where id = ? and order_group_id = ?
  |] (id, groupId)
  close conn

getProductCatalogue :: ByteString -> IO [ProductCatalogueEntry]
getProductCatalogue connectionString = do
  conn <- connectPostgreSQL connectionString
  rEntries <- query_ conn [sql|
    select ce.code
         , concat_ws(' ', nullif(btrim(ce.brand), '')
                        , nullif(btrim(ce."description"), '')
                        , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                        , nullif(btrim(ce."text"), ''))
         , ce.price as price_exc_vat
         , cast(round(ce.price * v.multiplier) as int) as price_inc_vat
         , ce.vat_rate
         , ce.biodynamic
         , ce.fair_trade
         , ce.gluten_free
         , ce.organic
         , ce.added_sugar
         , ce.vegan
         , ce.category
         , ce.brand
    from catalogue_entry ce
    inner join vat_rate v on ce.vat_rate = v.code
    order by ce.code
  |]
  close conn
  return $ (rEntries :: [ProductCatalogueEntryData]) <&> \(ProductCatalogueEntryData { pcedCode, pcedName, pcedPriceExcVat, pcedPriceIncVat, pcedVatRate, pcedBiodynamic, pcedFairTrade, pcedGlutenFree, pcedOrganic, pcedAddedSugar, pcedVegan, pcedCategory, pcedBrand }) -> ProductCatalogueEntry pcedCode pcedName pcedPriceExcVat pcedPriceIncVat pcedVatRate pcedBiodynamic pcedFairTrade pcedGlutenFree pcedOrganic pcedAddedSugar pcedVegan pcedCategory pcedBrand

replaceProductCatalogue :: ByteString -> UTCTime -> [ProductCatalogueData] -> IO ()
replaceProductCatalogue connectionString date entries = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    void $ execute_ conn [sql|
      truncate table catalogue_entry
    |]
    void $ executeMany conn [sql|
      insert into catalogue_entry (code, category, brand, "description", "text", size, price, vat_rate, rrp, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan, updated)
      values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |] entries
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
  close conn

acceptCatalogueUpdates :: ByteString -> Int -> UTCTime -> Int -> Int -> IO ()
acceptCatalogueUpdates connectionString groupId date orderId householdId = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    void $ execute conn [sql|
      update household_order set updated = ?
      where order_id = ? and household_id = ? and order_group_id = ?
    |] (date, orderId, householdId, groupId)
    void $ execute conn [sql|
      update household_order_item hoi set
        product_price_exc_vat = p.price, 
        product_price_inc_vat = cast(round(p.price * v.multiplier) as int),
        item_total_exc_vat = p.price * hoi.quantity,
        item_total_inc_vat = cast(round(p.price * v.multiplier) as int) * hoi.quantity
      from household_order_item hoi2
      inner join product p on p.id = hoi2.product_id
      inner join vat_rate v on v.code = p.vat_rate
      where hoi.order_id = hoi2.order_id and hoi.household_id = hoi2.household_id and hoi.product_id = hoi2.product_id
        and hoi.order_id = ? and hoi.household_id = ? and hoi.order_group_id = ?
    |] (orderId, householdId, groupId)
    void $ execute conn [sql|
      delete from household_order_item
      where order_id = ? and household_id = ? and order_group_id = ? and product_id in (
        select id from product where discontinued = true
      )
    |] (orderId, householdId, groupId)
  close conn
  
getGroup :: ByteString -> String -> IO (Maybe Int)
getGroup connectionString key = do
  conn <- connectPostgreSQL connectionString
  result <- query conn
    " select id\
    \ from order_group\
    \ where key = ?"
    (Only key)
  close conn
  return $ listToMaybe $ fmap fromOnly $ (result :: [Only Int])

getProductCatalogueCategories :: ByteString -> IO [String]
getProductCatalogueCategories connectionString = do
  conn <- connectPostgreSQL connectionString
  results <- query_ conn [sql|
    select distinct category
    from catalogue_entry ce
    order by category
  |]
  close conn
  return $ fmap fromOnly $ (results :: [Only String])

getProductCatalogueBrands :: ByteString -> IO [String]
getProductCatalogueBrands connectionString = do
  conn <- connectPostgreSQL connectionString
  results <- query_ conn [sql|
    select distinct brand
    from catalogue_entry ce
    order by brand
  |]
  close conn
  return $ fmap fromOnly $ (results :: [Only String])

getProductImage :: ByteString -> String -> IO (Maybe ByteString)
getProductImage connectionString code = do
  conn <- connectPostgreSQL connectionString
  results <- query conn [sql|
    select image
    from product_image
    where code = ?
  |] (Only code)
  close conn
  return $ listToMaybe $ fmap fromOnly $ (results :: [Only ByteString])

saveProductImage :: ByteString -> String -> ByteString -> IO ()
saveProductImage connectionString code image = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    insert into product_image (code, image)
    values (?, ?)
    ON CONFLICT (code) DO UPDATE SET image = EXCLUDED.image;
  |] (code, Binary image)
  close conn

getUploadedOrderFile :: ByteString -> String -> IO (Maybe ByteString)
getUploadedOrderFile connectionString fileId = do
  conn <- connectPostgreSQL connectionString
  results <- query conn [sql|
    select contents
    from order_file_upload
    where id = ?
  |] (Only fileId)
  close conn
  return $ listToMaybe $ fmap fromOnly $ (results :: [Only ByteString])

saveUploadedOrderFile :: ByteString -> String -> ByteString -> IO ()
saveUploadedOrderFile connectionString fileId fileContents = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    insert into order_file_upload (id, contents)
    values (?, ?)
    ON CONFLICT (id) DO UPDATE SET contents = EXCLUDED.contents;
  |] (fileId, Binary fileContents)
  close conn

deleteUploadedOrderFile :: ByteString -> String -> IO ()
deleteUploadedOrderFile connectionString fileId = do
  conn <- connectPostgreSQL connectionString
  void $ execute conn [sql|
    delete from order_file_upload
    where id = ?
  |] (Only fileId)
  close conn

reconcileOrderItem :: ByteString -> Int -> Int -> Int -> ReconcileOrderItemDetails -> IO ()
reconcileOrderItem connectionString groupId orderId productId details = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    forM_ (roidetHouseholdQuantities details) $ \h -> do
      let price = roidetProductPriceExcVat details
      let quantity = hqdetItemQuantity h
      let householdId = hqdetHouseholdId h

      void $ execute conn [sql|
        insert into order_item_adjustment (
            order_id
          , household_id
          , product_id
          , order_group_id
          , old_product_price_exc_vat
          , old_product_price_inc_vat
          , old_quantity
          , old_item_total_exc_vat
          , old_item_total_inc_vat
        )
        select
            order_id
          , household_id
          , product_id
          , order_group_id
          , product_price_exc_vat
          , product_price_inc_vat
          , quantity
          , item_total_exc_vat
          , item_total_inc_vat
        from past_household_order_item
        where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
        ON CONFLICT (order_id, household_id, product_id) DO NOTHING;
      |] (orderId, householdId, productId, groupId)

      void $ execute conn [sql|
        with new_values as (
          select ? as price, ? as quantity, ? as order_id, ? as household_id, ? as product_id, ? as order_group_id 
        )
        update past_household_order_item phoi
        set product_price_exc_vat = nv.price
          , product_price_inc_vat = cast(round(nv.price * v.multiplier) as int)
          , quantity = nv.quantity
          , item_total_exc_vat = nv.price * nv.quantity
          , item_total_inc_vat = cast(round(nv.price * v.multiplier) as int) * nv.quantity
        from past_household_order_item phoi2
        inner join new_values nv on phoi2.order_id = nv.order_id and phoi2.household_id = nv.household_id and phoi2.product_id = nv.product_id and phoi2.order_group_id = nv.order_group_id
        inner join product p on p.id = phoi2.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where phoi.order_id = phoi2.order_id and phoi.household_id = phoi2.household_id and phoi.product_id = phoi2.product_id and phoi.order_group_id = phoi2.order_group_id
      |] (price, quantity, orderId, householdId, productId, groupId)
  close conn

reconcileHouseholdOrderItems :: ByteString -> Int -> Int -> Int -> [ReconcileHouseholdOrderItemDetails] -> IO ()
reconcileHouseholdOrderItems connectionString groupId orderId householdId details = do
  conn <- connectPostgreSQL connectionString
  void $ withTransaction conn $ do
    forM_ details $ \d -> do
      let code = rhoidProductCode d
      let price = rhoidProductPriceExcVat d
      let quantity = rhoidQuantity d

      void $ execute conn [sql|
        insert into order_item_adjustment (
            order_id
          , household_id
          , product_id
          , order_group_id
          , old_product_price_exc_vat
          , old_product_price_inc_vat
          , old_quantity
          , old_item_total_exc_vat
          , old_item_total_inc_vat
        )
        select
            order_id
          , household_id
          , product_id
          , order_group_id
          , product_price_exc_vat
          , product_price_inc_vat
          , quantity
          , item_total_exc_vat
          , item_total_inc_vat
        from past_household_order_item
        where order_group_id = ? and order_id = ? and household_id = ? and product_code = ?
        ON CONFLICT (order_id, household_id, product_id) DO NOTHING;
      |] (groupId, orderId, householdId, code)

      void $ execute conn [sql|
        with new_values as (
          select ? as order_group_id 
               , ? as order_id
               , ? as household_id
               , ? :: text as product_code
               , ? as price
               , ? as quantity
        )
        update past_household_order_item phoi
        set product_price_exc_vat = nv.price
          , product_price_inc_vat = cast(round(nv.price * v.multiplier) as int)
          , quantity = nv.quantity
          , item_total_exc_vat = nv.price * nv.quantity
          , item_total_inc_vat = cast(round(nv.price * v.multiplier) as int) * nv.quantity
        from past_household_order_item phoi2
        inner join new_values nv on phoi2.order_group_id = nv.order_group_id
                                 and phoi2.order_id      = nv.order_id 
                                 and phoi2.household_id  = nv.household_id 
                                 and phoi2.product_code  = nv.product_code
        inner join product p on p.id = phoi2.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where phoi.order_group_id = phoi2.order_group_id 
          and phoi.order_id       = phoi2.order_id 
          and phoi.household_id   = phoi2.household_id 
          and phoi.product_id     = phoi2.product_id
      |] (groupId, orderId, householdId, code, price, quantity)

    let codes = map rhoidProductCode details
    missingProductIds <- query conn [sql|
      select product_id
      from past_household_order_item
      where order_group_id = ? and order_id = ? and household_id = ? and product_code not in ? 
    |] (groupId, orderId, householdId, In codes)

    forM_ (map fromOnly (missingProductIds :: [(Only Int)])) $ \productId -> do
      void $ execute conn [sql|
        insert into order_item_adjustment (
            order_id
          , household_id
          , product_id
          , order_group_id
          , old_product_price_exc_vat
          , old_product_price_inc_vat
          , old_quantity
          , old_item_total_exc_vat
          , old_item_total_inc_vat
        )
        select
            order_id
          , household_id
          , product_id
          , order_group_id
          , product_price_exc_vat
          , product_price_inc_vat
          , quantity
          , item_total_exc_vat
          , item_total_inc_vat
        from past_household_order_item
        where order_group_id = ? and order_id = ? and household_id = ? and product_id = ?
        ON CONFLICT (order_id, household_id, product_id) DO NOTHING;
      |] (groupId, orderId, householdId, productId)

      void $ execute conn [sql|
        update past_household_order_item phoi
        set quantity           = 0
          , item_total_exc_vat = 0
          , item_total_inc_vat = 0
        where phoi.order_group_id = ? 
          and phoi.order_id       = ? 
          and phoi.household_id   = ? 
          and phoi.product_id     = ?
      |] (groupId, orderId, householdId, productId)
  close conn

getGroupSettings :: ByteString -> Int -> IO GroupSettings
getGroupSettings connectionString groupId = do
  conn <- connectPostgreSQL connectionString
  rSettings <- query conn [sql|
    select enable_payments
    from order_group g
    where id = ?
  |] (Only groupId)
  close conn
  return $ fromMaybe (GroupSettings True) $ listToMaybe $ rSettings