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
                , replaceProductCatalogue
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
    toRow e = [toField $ ProductCatalogueData.code e, toField $ category e, toField $ brand e, toField $ description e, toField $ text e, toField $ size e, toField $ ProductCatalogueData.price e, toField $ ProductCatalogueData.vatRate e, toField $ rrp e, toField $ biodynamic e, toField $ fairTrade e, toField $ glutenFree e, toField $ organic e, toField $ addedSugar e, toField $ vegan e, toField $ priceChanged e]

  data HouseholdOrderItemData = HouseholdOrderItemData {
    hoi_orderId :: Int,
    hoi_householdId :: Int,
    hoi_productId :: Int,
    hoi_code :: String,
    hoi_name :: String,
    hoi_priceExcVat :: Int,
    hoi_priceIncVat :: Int,
    hoi_vatRate :: VatRate,
    hoi_quantity :: Int,
    hoi_itemTotalExcVat :: Int,
    hoi_itemTotalIncVat :: Int,
    hoi_discontinued :: Bool,
    hoi_newProductPriceExcVat :: Maybe Int,
    hoi_newProductPriceIncVat :: Maybe Int,
    hoi_newItemTotalExcVat :: Maybe Int,
    hoi_newItemTotalIncVat :: Maybe Int
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
    ho_totalExcVat :: Int, 
    ho_totalIncVat :: Int
    -- ho_newTotalExcVat :: Maybe Int, 
    -- ho_newTotalIncVat :: Maybe Int
  }

  instance FromRow HouseholdOrderData where
    fromRow = HouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  infixr 0 &
  
  getCollectiveOrder :: ByteString -> IO (Maybe CollectiveOrder)
  getCollectiveOrder connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        with orders as (
               select o.id, o.created_date, h.id as created_by_id, h.name as created_by_name, 
                 coalesce(bool_and(ho.complete), false) as complete
               from "order" o
               inner join household h on h.id = o.created_by_id
               left join household_order ho on ho.order_id = o.id
               group by o.id, o.created_date, h.id, h.name
               order by o.id desc
             )
        select o.id, o.created_date, o.created_by_id, o.created_by_name, o.complete, 
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat,
          -- case when max(p.updated) <= ho.updated then null 
          --      else coalesce(sum(case when p.updated <= ho.updated then hoi.item_total_exc_vat 
          --                             when p.discontinued then 0 
          --                             else p.price * hoi.quantity
          --                        end), 0) 
          -- end as new_total_exc_vat,
          -- case when max(p.updated) <= ho.updated then null 
          --      else coalesce(sum(case when p.updated <= ho.updated then hoi.item_total_inc_vat 
          --                             when p.discontinued then 0 
          --                             else cast(round(p.price * v.multiplier) as int) * hoi.quantity
          --                        end), 0) 
          end as new_total_inc_vat
        from orders o
        left join household_order ho on ho.order_id = o.id and ho.cancelled = false
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        left join vat_rate v on v.code = p.vat_rate
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.complete
        order by o.id desc
      |]
      is <- query_ conn [sql|
        select hoi.order_id
             , hoi.household_id
             , p.id
             , p.code
             , p.name
             , hoi.product_price_exc_vat
             , hoi.product_price_inc_vat
             , p.vat_rate
             , sum(hoi.quantity) as quantity
             , hoi.item_total_exc_vat
             , hoi.item_total_inc_vat
             , p.discontinued
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else p.price 
               end as new_product_price_exc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int)
               end as new_product_price_inc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else sum(p.price * hoi.quantity) 
               end as new_item_total_exc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else sum(cast(round(p.price * v.multiplier) as int) * hoi.quantity) 
               end as new_item_total_inc_vat
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        group by hoi.order_id, hoi.household_id, p.id, p.code, p.name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, p.vat_rate, hoi.item_total_exc_vat, hoi.item_total_inc_vat, p.discontinued, p.updated, ho.updated, p.price, v.multiplier
        order by p.code asc
      |]
      return (os :: [(Int, UTCTime, Int, String, Bool, Int, Int, Maybe Int, Maybe Int)], is :: [HouseholdOrderItemData])
    close conn
    return $ listToMaybe $ rOrders <&> \(id, created, createdBy, createdByName, complete, totalExcVat, totalIncVat, newTotalExcVat, newTotalIncVat) ->
      let item (HouseholdOrderItemData { hoi_productId, hoi_code, hoi_name, hoi_priceExcVat, hoi_priceIncVat, hoi_vatRate, hoi_quantity, hoi_itemTotalExcVat, hoi_itemTotalIncVat, hoi_discontinued, hoi_newProductPriceExcVat, hoi_newProductPriceIncVat, hoi_newItemTotalExcVat, hoi_newItemTotalIncVat }) = OrderItem hoi_productId hoi_code hoi_name hoi_priceExcVat hoi_priceIncVat hoi_vatRate hoi_quantity hoi_itemTotalExcVat hoi_itemTotalIncVat hoi_discontinued hoi_newProductPriceExcVat hoi_newProductPriceIncVat hoi_newItemTotalExcVat hoi_newItemTotalIncVat
          thisOrder (HouseholdOrderItemData { hoi_orderId }) = hoi_orderId == id
          items = map item $ filter thisOrder rItems
      in  collectiveOrder id created createdBy createdByName complete totalExcVat totalIncVat newTotalExcVat newTotalIncVat items
  
  getPastCollectiveOrders :: ByteString -> IO [PastCollectiveOrder]
  getPastCollectiveOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        select o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled, 
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat
        from past_order o
        left join past_household_order ho on ho.order_id = o.id and (o.cancelled or ho.cancelled = false)
        left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled
        order by o.id desc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, 
           sum(hoi.quantity) as quantity, 
           sum(hoi.item_total_exc_vat) as item_total_exc_vat, 
           sum(hoi.item_total_inc_vat) as item_total_inc_vat
        from past_household_order_item hoi
        group by hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate
        order by hoi.order_id, hoi.product_code asc
      |]
      return (os :: [(Int, UTCTime, Int, String, Bool, Int, Int)], is :: [(Int, Int, String, String, Int, Int, VatRate, Int, Int, Int)])
    close conn
    return $ rOrders <&> \(id, created, createdBy, createdByName, cancelled, totalExcVat, totalIncVat) ->
      let item (_, productId, code, name, priceExcVat, priceIncVat, vatRate, quantity, totalExcVat, totalIncVat) = PastOrderItem productId code name priceExcVat priceIncVat vatRate quantity totalExcVat totalIncVat
          thisOrder (oId, _, _, _, _, _, _, _, _, _) = oId == id
          items = map item $ filter thisOrder rItems
      in  pastCollectiveOrder id created createdBy createdByName cancelled totalExcVat totalIncVat items
  
  getHouseholdOrders :: ByteString -> IO [HouseholdOrder]
  getHouseholdOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        select o.id
             , o.created_date
             , cb.id as created_by_id
             , cb.name as created_by_name
             , h.id
             , h.name
             , ho.complete
             , ho.cancelled
             , coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat
             , coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat
             , case when max(p.updated) <= ho.updated then null 
                    else coalesce(sum(case when p.updated <= ho.updated then hoi.item_total_exc_vat 
                                           when p.discontinued then 0 
                                           else p.price * hoi.quantity
                                      end), 0) 
               end as new_total_exc_vat
             , case when max(p.updated) <= ho.updated then null 
                    else coalesce(sum(case when p.updated <= ho.updated then hoi.item_total_inc_vat 
                                           when p.discontinued then 0 
                                           else cast(round(p.price * v.multiplier) as int) * hoi.quantity
                                      end), 0) 
               end as new_total_inc_vat
        from household_order ho
        inner join "order" o on o.id = ho.order_id
        inner join household cb on cb.id = o.created_by_id
        inner join household h on h.id = ho.household_id
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        left join vat_rate v on v.code = p.vat_rate
        group by o.id, o.created_date, cb.id, cb.name, h.id, h.name, ho.complete, ho.cancelled, ho.updated
        order by o.id desc, h.name asc
      |]
      is <- query_ conn [sql|
        select hoi.order_id
             , hoi.household_id
             , p.id
             , p.code
             , p.name
             , hoi.product_price_exc_vat
             , hoi.product_price_inc_vat
             , p.vat_rate
             , hoi.quantity
             , hoi.item_total_exc_vat
             , hoi.item_total_inc_vat
             , p.discontinued
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else p.price 
               end as new_product_price_exc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int)
               end as new_product_price_inc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else p.price * hoi.quantity
               end as new_item_total_exc_vat
             , case when p.updated <= ho.updated then null 
                    when p.discontinued then 0 
                    else cast(round(p.price * v.multiplier) as int) * hoi.quantity
               end as new_item_total_inc_vat
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        order by hoi.ix
      |]
      return (os :: [HouseholdOrderData], is :: [HouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(HouseholdOrderData { ho_orderId, ho_orderCreated, ho_orderCreatedBy, ho_orderCreatedByName, ho_householdId, ho_householdName, ho_complete, ho_cancelled, ho_totalExcVat, ho_totalIncVat, ho_newTotalExcVat, ho_newTotalIncVat }) ->
      let item (HouseholdOrderItemData { hoi_productId, hoi_code, hoi_name, hoi_priceExcVat, hoi_priceIncVat, hoi_vatRate, hoi_quantity, hoi_itemTotalExcVat, hoi_itemTotalIncVat, hoi_discontinued, hoi_newProductPriceExcVat, hoi_newProductPriceIncVat, hoi_newItemTotalExcVat, hoi_newItemTotalIncVat }) = OrderItem hoi_productId hoi_code hoi_name hoi_priceExcVat hoi_priceIncVat hoi_vatRate hoi_quantity hoi_itemTotalExcVat hoi_itemTotalIncVat hoi_discontinued hoi_newProductPriceExcVat hoi_newProductPriceIncVat hoi_newItemTotalExcVat hoi_newItemTotalIncVat
          thisOrder (HouseholdOrderItemData { hoi_orderId, hoi_householdId }) = hoi_orderId == ho_orderId && hoi_householdId == ho_householdId
          items = map item $ filter thisOrder rItems
      in  householdOrder ho_orderId ho_orderCreated ho_orderCreatedBy ho_orderCreatedByName ho_householdId ho_householdName ho_complete ho_cancelled ho_totalExcVat ho_totalIncVat ho_newTotalExcVat ho_newTotalIncVat items

  getPastHouseholdOrders :: ByteString -> IO [PastHouseholdOrder]
  getPastHouseholdOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        select o.id, o.created_date, o.created_by_id, o.created_by_name, ho.household_id, ho.household_name, (case when o.cancelled then false else ho.cancelled end) as cancelled, 
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat
        from past_household_order ho
        inner join past_order o on o.id = ho.order_id
        left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, ho.household_id, ho.household_name, ho.cancelled
        order by o.id desc, ho.household_name asc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, hoi.household_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat
        from past_household_order_item hoi
        order by hoi.household_id, hoi.product_code
      |]
      return (os :: [(Int, UTCTime, Int, String, Int, String, Bool, Int, Int)], is :: [PastHouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(orderId, orderCreated, orderCreatedBy, orderCreatedByName, householdId, householdName, cancelled, totalExcVat, totalIncVat) ->
      let item (PastHouseholdOrderItemData { phoi_productId, phoi_code, phoi_name, phoi_priceExcVat, phoi_priceIncVat, phoi_vatRate, phoi_quantity, phoi_itemTotalExcVat, phoi_itemTotalIncVat}) = PastOrderItem phoi_productId phoi_code phoi_name phoi_priceExcVat phoi_priceIncVat phoi_vatRate phoi_quantity phoi_itemTotalExcVat phoi_itemTotalIncVat
          thisOrder (PastHouseholdOrderItemData { phoi_orderId, phoi_householdId }) = phoi_orderId == orderId && phoi_householdId == householdId
          items = map item $ filter thisOrder rItems
      in  pastHouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName cancelled totalExcVat totalIncVat items

  getHouseholds :: ByteString -> IO [Household]
  getHouseholds connectionString = do
    conn <- connectPostgreSQL connectionString
    rHouseholds <- query_ conn [sql|
        with household_total_orders as (
               select id, cast(sum(total) as int) as total
               from (
                 (select h.id, coalesce(sum(hoi.item_total_inc_vat), 0) as total
                 from household h
                 left join household_order ho on ho.household_id = h.id and ho.cancelled = false
                 left join household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = ho.order_id
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
      select h.id, h.name, hto.total as total_orders, htp.total as total_payments, htp.total - hto.total as balance
      from household h
      inner join household_total_orders hto on hto.id = h.id
      inner join household_total_payments htp on htp.id = h.id
      where h.archived = false
      order by h.name asc
    |]
    close conn
    return $ (rHouseholds :: [(Int, String, Int, Int, Int)]) <&> \(id, name, totalOrders, totalPayments, balance) -> Household id name totalOrders totalPayments balance

  getHouseholdPayments :: ByteString -> IO [HouseholdPayment]
  getHouseholdPayments connectionString = do
    conn <- connectPostgreSQL connectionString
    rPayments <- query_ conn [sql|
      select p.id, p.household_id, p."date", p.amount
      from household_payment p
      where p.archived = false
      order by p.id asc
    |]
    close conn
    return $ (rPayments :: [(Int, Int, UTCTime, Int)]) <&> \(id, householdId, date, amount) -> HouseholdPayment id householdId date amount

  createOrder :: ByteString -> UTCTime -> Int -> IO Int
  createOrder connectionString date householdId = do
    conn <- connectPostgreSQL connectionString
    id <- withTransaction conn $ do
      [Only id] <- query conn [sql|
        insert into "order" (created_date, created_by) values (?, ?) returning id
      |] (date, householdId)
      void $ execute conn [sql|
        insert into household_order (order_id, household_id, updated, complete, cancelled) values (?, ?, ?, false, false)
      |] (id, householdId, date)
      return id
    close conn
    return id

  deleteOrder :: ByteString -> Int -> IO ()
  deleteOrder connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from "order" where id = ?
    |] (Only orderId)
    close conn

  closeOrder :: ByteString -> Bool -> Int -> IO ()
  closeOrder connectionString cancelled orderId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql|
        insert into past_order (id, created_date, created_by_id, created_by_name, cancelled)
        select id, created_date, h.id, h.name, ?
        from "order" o
        inner join household h on h.id = o.created_by_id
        where id = ?
      |] (cancelled, orderId)
      execute conn [sql|
        insert into past_household_order (order_id, household_id, household_name, cancelled)
        select ho.order_id, h.id, h.name, ?
        from household_order ho
        inner join household h on h.id = ho.household_id
        where ho.order_id = ?
      |] (cancelled, orderId)
      execute conn [sql|
        insert into past_household_order_item (order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat)
        select hoi.order_id, hoi.household_id, hoi.product_id, p.code, p.name, hoi.product_price_exc_Vat, hoi.product_price_inc_vat, p.vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat
        from household_order_item hoi
        inner join product p on hoi.product_id = p.id
        inner join vat_rate v on v.code = p.vat_rate
        where hoi.order_id = ?
      |] (Only orderId)
      execute conn [sql|
        delete from household_order_item where order_id = ?
      |] (Only orderId)
      execute conn [sql|
        delete from household_order where order_id = ?
      |] (Only orderId)
      execute conn [sql|
        delete from "order" where id = ?
      |] (Only orderId)
    close conn

  createHouseholdOrder :: ByteString -> UTCTime -> Int -> Int -> IO ()
  createHouseholdOrder connectionString date orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      insert into household_order (order_id, household_id, updated, complete, cancelled) values (?, ?, ?, false, false)
    |] (orderId, householdId, date)
    close conn

  deleteHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  deleteHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  cancelHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  cancelHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql|
        update household_order set cancelled = true where order_id = ? and household_id = ?
      |] (orderId, householdId)
    close conn

  completeHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  completeHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set complete = true where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  reopenHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  reopenHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set cancelled = false, complete = false where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  ensureHouseholdOrderItem :: ByteString -> UTCTime -> Int -> Int -> String -> HouseholdOrderItemDetails -> IO ()
  ensureHouseholdOrderItem connectionString date orderId householdId productCode details = do
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
                   , ?
              from catalogue_entry ce
              where ce.code = ?
              returning id
            |] (date, productCode)
            return (id :: Int)
      exists <- query conn [sql|
        select 1 from household_order_item where order_id = ? and household_id = ? and product_id = ?
      |] (orderId, householdId, productId)
      if null (exists :: [Only Int]) then
        execute conn [sql|
          insert into household_order_item (order_id, household_id, product_id, product_price_exc_vat, product_price_inc_vat, quantity, item_total_exc_vat, item_total_inc_vat)
          select ?, ?, p.id, p.price as price_exc_Vat, cast(round(p.price * v.multiplier) as int) as price_inc_vat, ?, p.price * ? as item_total_exc_vat, cast(round(p.price * v.multiplier) as int) * ? as item_total_inc_vat
          from product p
          inner join vat_rate v on v.code = p.vat_rate
          where p.id = ?
        |] (orderId, householdId, quantity, quantity, quantity, productId)
      else
        execute conn [sql|
          update household_order_item hoi set 
            quantity = ?, 
            item_total_exc_vat = hoi.product_price_exc_vat * ?,
            item_total_inc_vat = hoi.product_price_inc_vat * ?
          where order_id = ? and household_id = ? and product_id = ?
        |] (quantity, quantity, quantity, orderId, householdId, productId)  
    close conn

  removeHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> IO ()
  removeHouseholdOrderItem connectionString orderId householdId productId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order_item where order_id = ? and household_id = ? and product_id = ?
    |] (orderId, householdId, productId)
    close conn

  createHousehold :: ByteString -> HouseholdDetails -> IO Int
  createHousehold connectionString details = do
    let name = hdName details
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household (name, archived) values (?, false) returning id
    |] (Only name)
    close conn
    return id
  
  updateHousehold :: ByteString -> Int -> HouseholdDetails -> IO ()
  updateHousehold connectionString householdId details = do
    let name = hdName details
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set name = ? where id = ?
    |] (name, householdId)
    close conn

  archiveHousehold :: ByteString -> Int -> IO ()
  archiveHousehold connectionString householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set archived = true where id = ?
    |] (Only householdId)
    close conn

  createHouseholdPayment :: ByteString -> Int -> HouseholdPaymentDetails -> IO Int
  createHouseholdPayment connectionString householdId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household_payment (household_id, "date", amount, archived) values (?, ?, ?, false) returning id
    |] (householdId, date, amount)
    close conn
    return id

  updateHouseholdPayment :: ByteString -> Int -> HouseholdPaymentDetails -> IO ()
  updateHouseholdPayment connectionString paymentId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_payment set "date" = ?, amount = ? where id = ?
    |] (date, amount, paymentId)
    close conn
  
  archiveHouseholdPayment :: ByteString -> Int -> IO ()
  archiveHouseholdPayment connectionString id = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_payment set archived = true where id = ?
    |] (Only id)
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
        insert into catalogue_entry (code, category, brand, "description", "text", size, price, vat_rate, rrp, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan, price_changed)
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