{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database ( getCollectiveOrder, getHouseholdOrders, getPastCollectiveOrders, getPastHouseholdOrders, getHouseholds, getHouseholdPayments, getProductCatalogue
                , createOrder, deleteOrder, closeOrder
                , createHouseholdOrder, deleteHouseholdOrder, cancelHouseholdOrder, completeHouseholdOrder, reopenHouseholdOrder
                , ensureHouseholdOrderItem, removeHouseholdOrderItem
                , createHousehold, updateHousehold, archiveHousehold
                , createHouseholdPayment, updateHouseholdPayment, archiveHouseholdPayment
                , replaceProductCatalogue, acceptCatalogueUpdates
                , getGroup
                ) where
  import Control.Monad (mzero, when, void)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.ToRow
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Database.PostgreSQL.Simple.SqlQQ
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.Time.Calendar (Day, showGregorian)
  import Data.Time.Clock (UTCTime)
  import CollectiveOrder
  import HouseholdOrder
  import PastCollectiveOrder
  import PastHouseholdOrder
  import PastOrderItem
  import Household
  import HouseholdPayment
  import ProductCatalogueData
  import ProductCatalogueEntry
  import OrderItem
  import Product (VatRate(..))
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  instance ToField VatRate where
    toField Zero = toDatabaseChar 'Z'
    toField Standard = toDatabaseChar 'S'
    toField Reduced = toDatabaseChar 'R'

  instance FromField VatRate where
    fromField f char = do
      c <- fromField f char
      case c of
        Just 'Z' -> return Zero
        Just 'S' -> return Standard
        Just 'R' -> return Reduced
        _ -> mzero

  instance FromRow ProductCatalogueData where
    fromRow = ProductCatalogueData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  instance ToRow ProductCatalogueData where
    toRow e = [toField $ ProductCatalogueData.code e, toField $ category e, toField $ brand e, toField $ description e, toField $ text e, toField $ size e, toField $ ProductCatalogueData.price e, toField $ ProductCatalogueData.vatRate e, toField $ rrp e, toField $ biodynamic e, toField $ fairTrade e, toField $ glutenFree e, toField $ organic e, toField $ addedSugar e, toField $ vegan e, toField $ updated e]

  data HouseholdOrderItemData = HouseholdOrderItemData {
    hoi_orderId :: Int,
    hoi_householdId :: Int,
    hoi_productId :: Int,
    hoi_code :: String,
    hoi_name :: String,
    hoi_oldPriceExcVat :: Maybe Int,
    hoi_oldPriceIncVat :: Maybe Int,
    hoi_vatRate :: VatRate,
    hoi_quantity :: Int,
    hoi_oldItemTotalExcVat :: Maybe Int,
    hoi_oldItemTotalIncVat :: Maybe Int,
    hoi_discontinued :: Bool,
    hoi_priceExcVat :: Int,
    hoi_priceIncVat :: Int,
    hoi_itemTotalExcVat :: Int,
    hoi_itemTotalIncVat :: Int
  }

  instance FromRow HouseholdOrderItemData where
    fromRow = HouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data PastHouseholdOrderItemData = PastHouseholdOrderItemData {
    phoi_orderId :: Int,
    phoi_householdId :: Int,
    phoi_productId :: Int,
    phoi_code :: String,
    phoi_name :: String,
    phoi_priceExcVat :: Int,
    phoi_priceIncVat :: Int,
    phoi_vatRate :: VatRate,
    phoi_quantity :: Int,
    phoi_itemTotalExcVat :: Int,
    phoi_itemTotalIncVat :: Int
  }

  instance FromRow PastHouseholdOrderItemData where
    fromRow = PastHouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data HouseholdOrderData = HouseholdOrderData {
    ho_orderId :: Int, 
    ho_orderCreated :: UTCTime, 
    ho_orderCreatedBy :: Int, 
    ho_orderCreatedByName :: String, 
    ho_householdId :: Int, 
    ho_householdName :: String, 
    ho_complete :: Bool, 
    ho_cancelled :: Bool, 
    ho_oldTotalExcVat :: Maybe Int, 
    ho_oldTotalIncVat :: Maybe Int,
    ho_totalExcVat :: Int, 
    ho_totalIncVat :: Int
  }

  instance FromRow HouseholdOrderData where
    fromRow = HouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  infixr 0 &
  
  getCollectiveOrder :: ByteString -> Int -> IO (Maybe CollectiveOrder)
  getCollectiveOrder connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
        with orders as (
          select o.id, o.created_date, h.id as created_by_id, h.name as created_by_name, 
            coalesce(bool_and(ho.complete), false) as complete
          from "order" o
          inner join household h on h.id = o.created_by_id
          left join household_order ho on ho.order_id = o.id
          where o.order_group_id = ?
          group by o.id, o.created_date, h.id, h.name
          order by o.id desc
        ),
        household_orders as (
          select ho.order_id, ho.household_id, max(p.updated) <= ho.updated as up_to_date,
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
          group by ho.order_id, ho.household_id
        )
        select o.id, o.created_date, o.created_by_id, o.created_by_name, o.complete, 
          case when bool_and(ho.up_to_date) then null 
               else cast(coalesce(sum(ho.old_total_exc_vat), 0) as int)
          end as old_total_exc_vat,
          case when bool_and(ho.up_to_date) then null 
               else cast(coalesce(sum(ho.old_total_inc_vat), 0) as int)
          end as old_total_inc_vat,
          cast(coalesce(sum(ho.total_exc_vat), 0) as int) as total_exc_vat,
          cast(coalesce(sum(ho.total_inc_vat), 0) as int) as new_total_inc_vat,
          coalesce(bool_and(ho.up_to_date or ho.total_inc_vat = ho.old_total_inc_vat), true) as all_up_to_date
        from orders o
        left join household_orders ho on ho.order_id = o.id
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.complete
        order by o.id desc
      |] (groupId, groupId)
      is <- query conn [sql|
        select hoi.order_id
             , hoi.household_id
             , p.id
             , p.code
             , p.name
             , case when p.updated <= ho.updated then null 
                    else hoi.product_price_exc_vat
               end as old_product_price_exc_vat
             , case when p.updated <= ho.updated then null 
                    else hoi.product_price_inc_vat
               end as old_product_price_inc_vat
             , p.vat_rate
             , sum(hoi.quantity) as quantity
             , case when p.updated <= ho.updated then null 
                    else hoi.item_total_exc_vat
               end as old_item_total_exc_vat
             , case when p.updated <= ho.updated then null 
                    else hoi.item_total_inc_vat
               end as old_item_total_inc_vat
             , p.discontinued
             , case when p.discontinued then 0 
                    else p.price 
               end as product_price_exc_vat
             , case when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int)
               end as product_price_inc_vat
             , case when p.discontinued then 0 
                    else sum(p.price * hoi.quantity) 
               end as item_total_exc_vat
             , case when p.discontinued then 0 
                    else sum(cast(round(p.price * v.multiplier) as int) * hoi.quantity) 
               end as item_total_inc_vat
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where ho.order_group_id = ? and not ho.cancelled
        group by hoi.order_id, hoi.household_id, p.id, p.code, p.name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, p.vat_rate, hoi.item_total_exc_vat, hoi.item_total_inc_vat, p.discontinued, p.updated, ho.updated, p.price, v.multiplier
        order by p.code asc
      |] (Only groupId)
      return (os :: [(Int, UTCTime, Int, String, Bool, Maybe Int, Maybe Int, Int, Int, Bool)], is :: [HouseholdOrderItemData])
    close conn
    return $ listToMaybe $ rOrders <&> \(id, created, createdBy, createdByName, complete, oldTotalExcVat, oldTotalIncVat, totalExcVat, totalIncVat, allUpToDate) ->
      let item (HouseholdOrderItemData { hoi_productId, hoi_code, hoi_name, hoi_oldPriceExcVat, hoi_oldPriceIncVat, hoi_vatRate, hoi_quantity, hoi_oldItemTotalExcVat, hoi_oldItemTotalIncVat, hoi_discontinued, hoi_priceExcVat, hoi_priceIncVat, hoi_itemTotalExcVat, hoi_itemTotalIncVat }) = OrderItem hoi_productId hoi_code hoi_name hoi_oldPriceExcVat hoi_oldPriceIncVat hoi_vatRate hoi_quantity hoi_oldItemTotalExcVat hoi_oldItemTotalIncVat hoi_discontinued hoi_priceExcVat hoi_priceIncVat hoi_itemTotalExcVat hoi_itemTotalIncVat
          thisOrder (HouseholdOrderItemData { hoi_orderId }) = hoi_orderId == id
          items = map item $ filter thisOrder rItems
      in  collectiveOrder id created createdBy createdByName complete oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat allUpToDate items
  
  getPastCollectiveOrders :: ByteString -> Int -> IO [PastCollectiveOrder]
  getPastCollectiveOrders connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
        select o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled, 
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat
        from past_order o
        left join past_household_order ho on ho.order_id = o.id and (o.cancelled or ho.cancelled = false)
        left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        where o.order_group_id = ?
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled
        order by o.id desc
      |] (Only groupId)
      is <- query conn [sql|
        select hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, 
           sum(hoi.quantity) as quantity, 
           sum(hoi.item_total_exc_vat) as item_total_exc_vat, 
           sum(hoi.item_total_inc_vat) as item_total_inc_vat
        from past_household_order_item hoi
        where hoi.order_group_id = ?
        group by hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate
        order by hoi.order_id, hoi.product_code asc
      |] (Only groupId)
      return (os :: [(Int, UTCTime, Int, String, Bool, Int, Int)], is :: [(Int, Int, String, String, Int, Int, VatRate, Int, Int, Int)])
    close conn
    return $ rOrders <&> \(id, created, createdBy, createdByName, cancelled, totalExcVat, totalIncVat) ->
      let item (_, productId, code, name, priceExcVat, priceIncVat, vatRate, quantity, totalExcVat, totalIncVat) = PastOrderItem productId code name priceExcVat priceIncVat vatRate quantity totalExcVat totalIncVat
          thisOrder (oId, _, _, _, _, _, _, _, _, _) = oId == id
          items = map item $ filter thisOrder rItems
      in  pastCollectiveOrder id created createdBy createdByName cancelled totalExcVat totalIncVat items
  
  getHouseholdOrders :: ByteString -> Int -> IO [HouseholdOrder]
  getHouseholdOrders connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
        select o.id
             , o.created_date
             , cb.id as created_by_id
             , cb.name as created_by_name
             , h.id
             , h.name
             , ho.complete
             , ho.cancelled
             , case when max(p.updated) is null or max(p.updated) <= ho.updated then null 
                    else coalesce(sum(hoi.item_total_exc_vat), 0)
               end as old_total_exc_vat
             , case when max(p.updated) is null or max(p.updated) <= ho.updated then null 
                    else coalesce(sum(hoi.item_total_inc_vat), 0)
               end as old_total_inc_vat
             , coalesce(sum(case when p.discontinued then 0 
                                 else p.price * hoi.quantity
                            end), 0) as total_exc_vat
             , coalesce(sum(case when p.discontinued then 0 
                                 else cast(round(p.price * v.multiplier) as int) * hoi.quantity
                            end), 0) as total_inc_vat
        from household_order ho
        inner join "order" o on o.id = ho.order_id
        inner join household cb on cb.id = o.created_by_id
        inner join household h on h.id = ho.household_id
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        left join vat_rate v on v.code = p.vat_rate
        where o.order_group_id = ?
        group by o.id, o.created_date, cb.id, cb.name, h.id, h.name, ho.complete, ho.cancelled, ho.updated
        order by o.id desc, h.name asc
      |] (Only groupId)
      is <- query conn [sql|
        select hoi.order_id
             , hoi.household_id
             , p.id
             , p.code
             , p.name
             , case when p.updated <= ho.updated then null 
                    else hoi.product_price_exc_vat
               end as old_product_price_exc_vat         
             , case when p.updated <= ho.updated then null 
                    else hoi.product_price_inc_vat
               end as old_product_price_inc_vat
             , p.vat_rate
             , hoi.quantity
             , case when p.updated <= ho.updated then null 
                    else hoi.item_total_exc_vat
               end as old_item_total_exc_vat
             , case when p.updated <= ho.updated then null 
                    else hoi.item_total_inc_vat
               end as old_item_total_inc_vat
             , p.discontinued
             , case when p.discontinued then 0 
                    else p.price 
               end as product_price_exc_vat
             , case when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int) 
               end as product_price_exc_vat
             , case when p.discontinued then 0 
                    else p.price * hoi.quantity
               end as item_total_exc_vat
             , case when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int) * hoi.quantity
               end as item_total_inc_vat
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where ho.order_group_id = ?
        order by hoi.ix
      |] (Only groupId)
      return (os :: [HouseholdOrderData], is :: [HouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(HouseholdOrderData { ho_orderId, ho_orderCreated, ho_orderCreatedBy, ho_orderCreatedByName, ho_householdId, ho_householdName, ho_complete, ho_cancelled, ho_oldTotalExcVat, ho_oldTotalIncVat, ho_totalExcVat, ho_totalIncVat }) ->
      let item (HouseholdOrderItemData { hoi_productId, hoi_code, hoi_name, hoi_oldPriceExcVat, hoi_oldPriceIncVat, hoi_vatRate, hoi_quantity, hoi_oldItemTotalExcVat, hoi_oldItemTotalIncVat, hoi_discontinued, hoi_priceExcVat, hoi_priceIncVat, hoi_itemTotalExcVat, hoi_itemTotalIncVat }) = OrderItem hoi_productId hoi_code hoi_name hoi_oldPriceExcVat hoi_oldPriceIncVat hoi_vatRate hoi_quantity hoi_oldItemTotalExcVat hoi_oldItemTotalIncVat hoi_discontinued hoi_priceExcVat hoi_priceIncVat hoi_itemTotalExcVat hoi_itemTotalIncVat
          thisOrder (HouseholdOrderItemData { hoi_orderId, hoi_householdId }) = hoi_orderId == ho_orderId && hoi_householdId == ho_householdId
          items = map item $ filter thisOrder rItems
      in  householdOrder ho_orderId ho_orderCreated ho_orderCreatedBy ho_orderCreatedByName ho_householdId ho_householdName ho_complete ho_cancelled ho_oldTotalExcVat ho_oldTotalIncVat ho_totalExcVat ho_totalIncVat items

  getPastHouseholdOrders :: ByteString -> Int -> IO [PastHouseholdOrder]
  getPastHouseholdOrders connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
        select o.id, o.created_date, o.created_by_id, o.created_by_name, ho.household_id, ho.household_name, (case when o.cancelled then true else ho.cancelled end) as cancelled, 
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat
        from past_household_order ho
        inner join past_order o on o.id = ho.order_id
        left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        where o.order_group_id = ?
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, ho.household_id, ho.household_name, ho.cancelled
        order by o.id desc, ho.household_name asc
      |] (Only groupId)
      is <- query conn [sql|
        select hoi.order_id, hoi.household_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat
        from past_household_order_item hoi
        where hoi.order_group_id = ?
        order by hoi.household_id, hoi.product_code
      |] (Only groupId)
      return (os :: [(Int, UTCTime, Int, String, Int, String, Bool, Int, Int)], is :: [PastHouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(orderId, orderCreated, orderCreatedBy, orderCreatedByName, householdId, householdName, cancelled, totalExcVat, totalIncVat) ->
      let item (PastHouseholdOrderItemData { phoi_productId, phoi_code, phoi_name, phoi_priceExcVat, phoi_priceIncVat, phoi_vatRate, phoi_quantity, phoi_itemTotalExcVat, phoi_itemTotalIncVat}) = PastOrderItem phoi_productId phoi_code phoi_name phoi_priceExcVat phoi_priceIncVat phoi_vatRate phoi_quantity phoi_itemTotalExcVat phoi_itemTotalIncVat
          thisOrder (PastHouseholdOrderItemData { phoi_orderId, phoi_householdId }) = phoi_orderId == orderId && phoi_householdId == householdId
          items = map item $ filter thisOrder rItems
      in  pastHouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName cancelled totalExcVat totalIncVat items

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
              left join past_household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = ho.order_id
              group by h.id)
            ) as h
            group by id
          ),
          household_total_payments as (
            select h.id, coalesce(sum(hp.amount), 0) as total
            from household h
            left join household_payment hp on hp.household_id = h.id
            group by h.id
          )
      select h.id, h.name, h.contact_name, h.contact_email, h.contact_phone, hto.total as total_orders, htp.total as total_payments, htp.total - hto.total as balance
      from household h
      inner join household_total_orders hto on hto.id = h.id
      inner join household_total_payments htp on htp.id = h.id
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

  createOrder :: ByteString -> Int -> UTCTime -> Int -> IO Int
  createOrder connectionString groupId date householdId = do
    conn <- connectPostgreSQL connectionString
    id <- withTransaction conn $ do
      [Only id] <- query conn [sql|
        insert into "order" (order_group_id, created_date, created_by_id) values (?, ?, ?) returning id
      |] (groupId, date, householdId)
      void $ execute conn [sql|
        insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
      |] (groupId, id, householdId, date)
      return id
    close conn
    return id

  deleteOrder :: ByteString -> Int -> Int -> IO ()
  deleteOrder connectionString groupId orderId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from "order" where id = ? and order_group_id = ?
    |] (orderId, groupId)
    close conn

  closeOrder :: ByteString -> Int -> Bool -> Int -> IO ()
  closeOrder connectionString groupId cancelled orderId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql|
        insert into past_order (order_group_id, id, created_date, created_by_id, created_by_name, cancelled)
        select o.order_group_id, o.id, o.created_date, h.id, h.name, ?
        from "order" o
        inner join household h on h.id = o.created_by_id
        where o.id = ? and o.order_group_id = ?
      |] (cancelled, orderId, groupId)
      execute conn [sql|
        insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled)
        select ho.order_group_id, ho.order_id, h.id, h.name, ?
        from household_order ho
        inner join household h on h.id = ho.household_id
        where ho.order_id = ? and ho.order_group_id = ?
      |] (cancelled, orderId, groupId)
      execute conn [sql|
        insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat)
        select hoi.order_group_id, hoi.order_id, hoi.household_id, hoi.product_id, p.code, p.name, hoi.product_price_exc_Vat, hoi.product_price_inc_vat, p.vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat
        from household_order_item hoi
        inner join product p on hoi.product_id = p.id
        inner join vat_rate v on v.code = p.vat_rate
        where hoi.order_id = ? and hoi.order_group_id = ?
      |] (orderId, groupId)
      execute conn [sql|
        delete from household_order_item where order_id = ? and order_group_id = ?
      |] (orderId, groupId)
      execute conn [sql|
        delete from household_order where order_id = ? and order_group_id = ?
      |] (orderId, groupId)
      execute conn [sql|
        delete from "order" where id = ? and order_group_id = ?
      |] (orderId, groupId)
    close conn

  createHouseholdOrder :: ByteString -> Int -> UTCTime -> Int -> Int -> IO ()
  createHouseholdOrder connectionString groupId date orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
    |] (groupId, orderId, householdId, date)
    close conn

  deleteHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
  deleteHouseholdOrder connectionString groupId orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order where order_id = ? and household_id = ? and order_group_id = ?
    |] (orderId, householdId, groupId)
    close conn

  cancelHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
  cancelHouseholdOrder connectionString groupId orderId householdId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql|
        update household_order set cancelled = true where order_id = ? and household_id = ? and order_group_id = ?
      |] (orderId, householdId, groupId)
    close conn

  completeHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
  completeHouseholdOrder connectionString groupId orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set complete = true where order_id = ? and household_id = ? and order_group_id = ?
    |] (orderId, householdId, groupId)
    close conn

  reopenHouseholdOrder :: ByteString -> Int -> Int -> Int -> IO ()
  reopenHouseholdOrder connectionString groupId orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set cancelled = false, complete = false where order_id = ? and household_id = ? and order_group_id = ?
    |] (orderId, householdId, groupId)
    close conn

  ensureHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> String -> HouseholdOrderItemDetails -> IO ()
  ensureHouseholdOrderItem connectionString groupId orderId householdId productCode details = do
    let quantity = hoidQuantity details
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      productId <- do
        ids <- query conn [sql|
          select id from product where code = ?
        |] (Only productCode)
        case ids of
          (Only id):xs -> return id
          _ -> do
            [Only id] <- query conn [sql|
              insert into product ("code", "name", price, vat_rate, discontinued, updated) 
              select ce.code
                   , concat_ws(' ', nullif(btrim(ce.brand), '')
                                  , nullif(btrim(ce."description"), '')
                                  , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                                  , nullif(btrim(ce."text"), ''))
                   , ce.price
                   , ce.vat_rate
                   , false
                   , ce.updated
              from catalogue_entry ce
              where ce.code = ?
              returning id
            |] (Only productCode)
            return (id :: Int)
      exists <- query conn [sql|
        select 1 from household_order_item where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
      |] (orderId, householdId, productId, groupId)
      if null (exists :: [Only Int]) then
        execute conn [sql|
          insert into household_order_item (order_group_id, order_id, household_id, product_id, product_price_exc_vat, product_price_inc_vat, quantity, item_total_exc_vat, item_total_inc_vat)
          select ?, ?, ?, p.id, p.price as price_exc_Vat, cast(round(p.price * v.multiplier) as int) as price_inc_vat, ?, p.price * ? as item_total_exc_vat, cast(round(p.price * v.multiplier) as int) * ? as item_total_inc_vat
          from product p
          inner join vat_rate v on v.code = p.vat_rate
          where p.id = ?
        |] (groupId, orderId, householdId, quantity, quantity, quantity, productId)
      else
        execute conn [sql|
          update household_order_item hoi set 
            quantity = ?, 
            item_total_exc_vat = hoi.product_price_exc_vat * ?,
            item_total_inc_vat = hoi.product_price_inc_vat * ?
          where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
        |] (quantity, quantity, quantity, orderId, householdId, productId, groupId)  
    close conn

  removeHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> Int -> IO ()
  removeHouseholdOrderItem connectionString groupId orderId householdId productId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order_item where order_id = ? and household_id = ? and product_id = ? and order_group_id = ?
    |] (orderId, householdId, productId, groupId)
    close conn

  createHousehold :: ByteString -> Int -> HouseholdDetails -> IO Int
  createHousehold connectionString groupId details = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household (order_group_id, name, contact_name, contact_email, contact_phone, archived) values (?, ?, ?, ?, ?, false) returning id
    |] (groupId, hdName details, hdContactName details, hdContactEmail details, hdContactPhone details)
    close conn
    return id
  
  updateHousehold :: ByteString -> Int -> Int -> HouseholdDetails -> IO ()
  updateHousehold connectionString groupId householdId details = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set 
        name = ?, contact_name = ?, contact_email = ?, contact_phone = ? 
      where id = ? and order_group_id = ?
    |] (hdName details, hdContactName details, hdContactEmail details, hdContactPhone details, householdId, groupId)
    close conn

  archiveHousehold :: ByteString -> Int -> Int -> IO ()
  archiveHousehold connectionString groupId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set archived = true where id = ? and order_group_id = ?
    |] (householdId, groupId)
    close conn

  createHouseholdPayment :: ByteString -> Int -> Int -> HouseholdPaymentDetails -> IO Int
  createHouseholdPayment connectionString groupId householdId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household_payment (order_group_id, household_id, "date", amount, archived) values (?, ?, ?, ?, false) returning id
    |] (groupId, householdId, date, amount)
    close conn
    return id

  updateHouseholdPayment :: ByteString -> Int -> Int -> HouseholdPaymentDetails -> IO ()
  updateHouseholdPayment connectionString groupId paymentId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_payment set "date" = ?, amount = ? where id = ? and order_group_id = ?
    |] (date, amount, paymentId, groupId)
    close conn
  
  archiveHouseholdPayment :: ByteString -> Int -> Int -> IO ()
  archiveHouseholdPayment connectionString groupId id = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
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
      from catalogue_entry ce
      inner join vat_rate v on ce.vat_rate = v.code
    |]
    close conn
    return $ (rEntries :: [(String, String, Int, Int, VatRate)]) <&> \(code, name, priceExcVat, priceIncVat, vatRate) -> ProductCatalogueEntry code name priceExcVat priceIncVat vatRate

  replaceProductCatalogue :: ByteString -> UTCTime -> [ProductCatalogueData] -> IO ()
  replaceProductCatalogue connectionString date entries = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute_ conn [sql|
        truncate table catalogue_entry
      |]
      void $ executeMany conn [sql|
        insert into catalogue_entry (code, category, brand, "description", "text", size, price, vat_rate, rrp, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan, updated)
        values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |] entries
      execute conn [sql|
        update product
        set "name" = case when ce.code is null then p."name" else concat_ws(' ', nullif(btrim(ce.brand), '')
                                                                               , nullif(btrim(ce."description"), '')
                                                                               , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                                                                               , nullif(btrim(ce."text"), '')) end
          , price = coalesce(ce.price, p.price)
          , vat_rate = coalesce(ce.vat_rate, p.vat_rate)
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
    withTransaction conn $ do
      execute conn [sql|
        update household_order set updated = ?
        where order_id = ? and household_id = ? and order_group_id = ?
      |] (date, orderId, householdId, groupId)
      execute conn [sql|
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
      execute conn [sql|
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