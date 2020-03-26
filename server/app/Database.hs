{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database ( getCollectiveOrder, getHouseholdOrders, getPastCollectiveOrders, getPastHouseholdOrders, getHouseholds, getHouseholdPayments, getProductCatalogue
                , createOrder, closeOrder
                , cancelHouseholdOrder, completeHouseholdOrder, reopenHouseholdOrder
                , ensureHouseholdOrderItem, ensureAllItemsFromPastHouseholdOrder, removeHouseholdOrderItem
                , createHousehold, updateHousehold, archiveHousehold
                , createHouseholdPayment, updateHouseholdPayment, archiveHouseholdPayment
                , replaceProductCatalogue, acceptCatalogueUpdates
                , getProductCatalogueCategories, getProductCatalogueBrands, getProductImage, saveProductImage
                , getGroup
                , reconcileOrderItem
                , getGroupSettings
                ) where
  import Control.Monad (mzero, when, void, forM_)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.ToRow
  import Database.PostgreSQL.Simple.FromField (FromField(..))
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Database.PostgreSQL.Simple.SqlQQ
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust, fromMaybe)
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
  import Household
  import HouseholdPayment
  import ProductCatalogueData
  import ProductCatalogueEntry
  import OrderItem
  import Product (VatRate(..))
  import GroupSettings
  
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
    toRow e = [toField $ ProductCatalogueData.code e, toField $ ProductCatalogueData.category e, toField $ ProductCatalogueData.brand e, toField $ ProductCatalogueData.description e, toField $ ProductCatalogueData.text e, toField $ ProductCatalogueData.size e, toField $ ProductCatalogueData.price e, toField $ ProductCatalogueData.vatRate e, toField $ ProductCatalogueData.rrp e, toField $ ProductCatalogueData.biodynamic e, toField $ ProductCatalogueData.fairTrade e, toField $ ProductCatalogueData.glutenFree e, toField $ ProductCatalogueData.organic e, toField $ ProductCatalogueData.addedSugar e, toField $ ProductCatalogueData.vegan e, toField $ updated e]

  data HouseholdOrderItemData = HouseholdOrderItemData {
    hoi_orderId :: Int,
    hoi_householdId :: Int,
    hoi_productId :: Int,
    hoi_code :: String,
    hoi_name :: String,
    hoi_oldPriceExcVat :: Int,
    hoi_oldPriceIncVat :: Int,
    hoi_vatRate :: VatRate,
    hoi_quantity :: Int,
    hoi_oldItemTotalExcVat :: Int,
    hoi_oldItemTotalIncVat :: Int,
    hoi_discontinued :: Bool,
    hoi_priceExcVat :: Int,
    hoi_priceIncVat :: Int,
    hoi_itemTotalExcVat :: Int,
    hoi_itemTotalIncVat :: Int,
    hoi_b :: Bool,
    hoi_f :: Bool,
    hoi_g :: Bool,
    hoi_o :: Bool,
    hoi_s :: Bool,
    hoi_v :: Bool,
    hoi_updated :: Bool
  }

  instance FromRow HouseholdOrderItemData where
    fromRow = HouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data OrderItemData = OrderItemData {
    oi_orderId :: Int,
    oi_productId :: Int,
    oi_code :: String,
    oi_name :: String,
    oi_vatRate :: VatRate,
    oi_quantity :: Int,
    oi_priceExcVat :: Int,
    oi_priceIncVat :: Int,
    oi_itemTotalExcVat :: Int,
    oi_itemTotalIncVat :: Int,
    oi_b :: Bool,
    oi_f :: Bool,
    oi_g :: Bool,
    oi_o :: Bool,
    oi_s :: Bool,
    oi_v :: Bool
  }

  instance FromRow OrderItemData where
    fromRow = OrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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
    phoi_itemTotalIncVat :: Int,
    phoi_b :: Bool,
    phoi_f :: Bool,
    phoi_g :: Bool,
    phoi_o :: Bool,
    phoi_s :: Bool,
    phoi_v :: Bool,
    phoi_oldProductPriceExcVat :: Maybe Int,
    phoi_oldProductPriceIncVat :: Maybe Int,
    phoi_oldQuantity :: Maybe Int,
    phoi_oldItemTotalExcVat :: Maybe Int,
    phoi_oldItemTotalIncVat :: Maybe Int
  }

  instance FromRow PastHouseholdOrderItemData where
    fromRow = PastHouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data PastOrderItemData = PastOrderItemData {
    poi_orderId :: Int,
    poi_productId :: Int,
    poi_code :: String,
    poi_name :: String,
    poi_priceExcVat :: Int,
    poi_priceIncVat :: Int,
    poi_vatRate :: VatRate,
    poi_quantity :: Int,
    poi_itemTotalExcVat :: Int,
    poi_itemTotalIncVat :: Int,
    poi_b :: Bool,
    poi_f :: Bool,
    poi_g :: Bool,
    poi_o :: Bool,
    poi_s :: Bool,
    poi_v :: Bool,
    poi_oldProductPriceExcVat :: Maybe Int,
    poi_oldProductPriceIncVat :: Maybe Int,
    poi_oldQuantity :: Maybe Int,
    poi_oldItemTotalExcVat :: Maybe Int,
    poi_oldItemTotalIncVat :: Maybe Int
  }

  instance FromRow PastOrderItemData where
    fromRow = PastOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data HouseholdOrderData = HouseholdOrderData {
    ho_orderId :: Int, 
    ho_orderCreated :: UTCTime, 
    ho_orderCreatedBy :: Int, 
    ho_orderCreatedByName :: String, 
    ho_householdId :: Int, 
    ho_householdName :: String, 
    ho_complete :: Bool, 
    ho_cancelled :: Bool, 
    ho_oldTotalExcVat :: Int, 
    ho_oldTotalIncVat :: Int,
    ho_totalExcVat :: Int, 
    ho_totalIncVat :: Int,
    ho_updated :: Bool
  }

  instance FromRow HouseholdOrderData where
    fromRow = HouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  data PastCollectiveOrderData = PastCollectiveOrderData {
    pco_orderId :: Int, 
    pco_orderCreated :: UTCTime, 
    pco_orderCreatedBy :: Int, 
    pco_orderCreatedByName :: String, 
    pco_cancelled :: Bool, 
    pco_reconciled :: Bool, 
    pco_totalExcVat :: Int, 
    pco_totalIncVat :: Int,
    pco_oldTotalExcVat :: Maybe Int, 
    pco_oldTotalIncVat :: Maybe Int
  }
  
  instance FromRow PastCollectiveOrderData where
    fromRow = PastCollectiveOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 

  data PastHouseholdOrderData = PastHouseholdOrderData {
    pho_orderId :: Int, 
    pho_orderCreated :: UTCTime, 
    pho_orderCreatedBy :: Int, 
    pho_orderCreatedByName :: String, 
    pho_orderAbandoned :: Bool,
    pho_householdId :: Int, 
    pho_householdName :: String, 
    pho_cancelled :: Bool, 
    pho_reconciled :: Bool, 
    pho_totalExcVat :: Int, 
    pho_totalIncVat :: Int,
    pho_oldTotalExcVat :: Maybe Int, 
    pho_oldTotalIncVat :: Maybe Int
  }
  
  instance FromRow PastHouseholdOrderData where
    fromRow = PastHouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 

  data ProductCatalogueEntryData = ProductCatalogueEntryData {
    pce_code :: String,
    pce_name :: String,
    pce_priceExcVat :: Int,
    pce_priceIncVat :: Int,
    pce_vatRate :: VatRate,
    pce_biodynamic :: Bool,
    pce_fairTrade :: Bool,
    pce_glutenFree :: Bool,
    pce_organic :: Bool,
    pce_addedSugar :: Bool,
    pce_vegan :: Bool,
    pce_category :: String,
    pce_brand :: String
  }

  instance FromRow ProductCatalogueEntryData where
    fromRow = ProductCatalogueEntryData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  instance FromRow GroupSettings where
    fromRow = GroupSettings <$> field

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
          cast(coalesce(sum(ho.old_total_exc_vat), 0) as int) as old_total_exc_vat,
          cast(coalesce(sum(ho.old_total_inc_vat), 0) as int) as old_total_inc_vat,
          cast(coalesce(sum(ho.total_exc_vat), 0) as int) as total_exc_vat,
          cast(coalesce(sum(ho.total_inc_vat), 0) as int) as new_total_inc_vat,
          coalesce(bool_and(ho.up_to_date), true) as all_up_to_date
        from orders o
        left join household_orders ho on ho.order_id = o.id
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.complete
        order by o.id desc
      |] (groupId, groupId)
      is <- query conn [sql|
        select hoi.order_id
             , p.id
             , p.code
             , p.name
             , p.vat_rate
             , sum(hoi.quantity) as quantity
             , p.price as product_price_exc_vat
             , cast(round(p.price * v.multiplier) as int) as product_price_inc_vat
             , sum(p.price * hoi.quantity) as item_total_exc_vat
             , sum(cast(round(p.price * v.multiplier) as int) * hoi.quantity) as item_total_inc_vat
             , p.biodynamic
             , p.fair_trade
             , p.gluten_free
             , p.organic
             , p.added_sugar
             , p.vegan
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where ho.order_group_id = ? and not ho.cancelled
        group by hoi.order_id, p.id, p.code, p.name, p.vat_rate, p.price, v.multiplier, p.biodynamic, p.fair_trade, p.gluten_free, p.organic, p.added_sugar, p.vegan
        order by p.code asc
      |] (Only groupId)
      return (os :: [(Int, UTCTime, Int, String, Bool, Int, Int, Int, Int, Bool)], is :: [OrderItemData])
    close conn
    return $ listToMaybe $ rOrders <&> \(id, created, createdBy, createdByName, complete, oldTotalExcVat, oldTotalIncVat, totalExcVat, totalIncVat, allUpToDate) ->
      let item (OrderItemData { oi_productId, oi_code, oi_name, oi_vatRate, oi_quantity, oi_priceExcVat, oi_priceIncVat, oi_itemTotalExcVat, oi_itemTotalIncVat, oi_b, oi_f, oi_g, oi_o, oi_s, oi_v }) 
            = orderItem oi_productId oi_code oi_name oi_vatRate oi_priceExcVat oi_priceIncVat oi_quantity oi_itemTotalExcVat oi_itemTotalIncVat oi_b oi_f oi_g oi_o oi_s oi_v
          thisOrder (OrderItemData { oi_orderId }) = oi_orderId == id
          items = map item $ filter thisOrder rItems
      in  collectiveOrder id created createdBy createdByName complete oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat allUpToDate items
  
  getPastCollectiveOrders :: ByteString -> Int -> IO [PastCollectiveOrder]
  getPastCollectiveOrders connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
        select o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled, 
          bool_and(adj.order_id is not null) as reconciled,
          coalesce(sum(hoi.item_total_exc_vat), 0) as total_exc_vat, 
          coalesce(sum(hoi.item_total_inc_vat), 0) as total_inc_vat,
          sum(coalesce(adj.old_item_total_exc_vat, hoi.item_total_exc_vat)) as old_total_exc_vat,
          sum(coalesce(adj.old_item_total_inc_vat, hoi.item_total_inc_vat)) as old_total_inc_vat
        from past_order o
        left join past_household_order ho on ho.order_id = o.id and (o.cancelled or ho.cancelled = false)
        left join past_household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
        where o.order_group_id = ?
        group by o.id, o.created_date, o.created_by_id, o.created_by_name, o.cancelled
        order by o.id desc
      |] (Only groupId)
      is <- query conn [sql|
        select hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, 
           sum(hoi.quantity) as quantity, 
           sum(hoi.item_total_exc_vat) as item_total_exc_vat, 
           sum(hoi.item_total_inc_vat) as item_total_inc_vat,
           hoi.product_biodynamic,
           hoi.product_fair_trade,
           hoi.product_gluten_free,
           hoi.product_organic,
           hoi.product_added_sugar,
           hoi.product_vegan,
           max(adj.old_product_price_exc_vat) as old_product_price_exc_vat,
           max(adj.old_product_price_inc_vat) as old_product_price_inc_vat,
           sum(adj.old_quantity) as old_quantity,
           sum(adj.old_item_total_exc_vat) as old_item_total_exc_vat,
           sum(adj.old_item_total_inc_vat) as old_item_total_inc_vat
        from past_household_order_item hoi
        left join order_item_adjustment adj on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
        where hoi.order_group_id = ?
        group by hoi.order_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, hoi.product_biodynamic, hoi.product_fair_trade, hoi.product_gluten_free, hoi.product_organic, hoi.product_added_sugar, hoi.product_vegan
        order by hoi.order_id, hoi.product_code asc
      |] (Only groupId)
      return (os :: [PastCollectiveOrderData], is :: [PastOrderItemData])
    close conn
    return $ rOrders <&> \(PastCollectiveOrderData { pco_orderId, pco_orderCreated, pco_orderCreatedBy, pco_orderCreatedByName, pco_cancelled, pco_reconciled, pco_totalExcVat, pco_totalIncVat, pco_oldTotalExcVat, pco_oldTotalIncVat }) ->
      let item (PastOrderItemData { poi_productId, poi_code, poi_name, poi_priceExcVat, poi_priceIncVat, poi_vatRate, poi_quantity, poi_itemTotalExcVat, poi_itemTotalIncVat, poi_b, poi_f, poi_g, poi_o, poi_s, poi_v, poi_oldProductPriceExcVat, poi_oldProductPriceIncVat, poi_oldQuantity, poi_oldItemTotalExcVat, poi_oldItemTotalIncVat }) 
            = householdOrderItem poi_productId poi_code poi_name poi_vatRate poi_priceExcVat poi_priceIncVat  poi_quantity poi_itemTotalExcVat poi_itemTotalIncVat poi_b poi_f poi_g poi_o poi_s poi_v poi_oldProductPriceExcVat poi_oldProductPriceIncVat poi_oldQuantity poi_oldItemTotalExcVat poi_oldItemTotalIncVat (Just False)
          thisOrder (PastOrderItemData { poi_orderId }) = poi_orderId == pco_orderId
          items = map item $ filter thisOrder rItems
      in  pastCollectiveOrder pco_orderId pco_orderCreated pco_orderCreatedBy pco_orderCreatedByName pco_cancelled pco_reconciled pco_totalExcVat pco_totalIncVat pco_oldTotalExcVat pco_oldTotalIncVat items
  
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
             , hoi.product_price_exc_vat as old_product_price_exc_vat         
             , hoi.product_price_inc_vat as old_product_price_inc_vat
             , p.vat_rate
             , hoi.quantity
             , hoi.item_total_exc_vat as old_item_total_exc_vat
             , hoi.item_total_inc_vat as old_item_total_inc_vat
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
             , p.biodynamic
             , p.fair_trade
             , p.gluten_free
             , p.organic
             , p.added_sugar
             , p.vegan
             , p.updated > ho.updated as updated
        from household_order_item hoi
        inner join household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
        inner join product p on p.id = hoi.product_id
        inner join vat_rate v on v.code = p.vat_rate
        where ho.order_group_id = ?
        order by hoi.ix
      |] (Only groupId)
      return (os :: [HouseholdOrderData], is :: [HouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(HouseholdOrderData { ho_orderId, ho_orderCreated, ho_orderCreatedBy, ho_orderCreatedByName, ho_householdId, ho_householdName, ho_complete, ho_cancelled, ho_oldTotalExcVat, ho_oldTotalIncVat, ho_totalExcVat, ho_totalIncVat, ho_updated }) ->
      let item (HouseholdOrderItemData { hoi_productId, hoi_code, hoi_name, hoi_oldPriceExcVat, hoi_oldPriceIncVat, hoi_vatRate, hoi_quantity, hoi_oldItemTotalExcVat, hoi_oldItemTotalIncVat, hoi_discontinued, hoi_priceExcVat, hoi_priceIncVat, hoi_itemTotalExcVat, hoi_itemTotalIncVat, hoi_b, hoi_f, hoi_g, hoi_o, hoi_s, hoi_v, hoi_updated }) 
            = householdOrderItem hoi_productId hoi_code hoi_name hoi_vatRate hoi_priceExcVat hoi_priceIncVat hoi_quantity hoi_itemTotalExcVat hoi_itemTotalIncVat hoi_b hoi_f hoi_g hoi_o hoi_s hoi_v (if ho_updated then (Just hoi_oldPriceExcVat) else Nothing) (if ho_updated then (Just hoi_oldPriceIncVat) else Nothing) (if ho_updated then (Just hoi_quantity) else Nothing) (if ho_updated then (Just hoi_oldItemTotalExcVat) else Nothing) (if ho_updated then (Just hoi_oldItemTotalIncVat) else Nothing) (if ho_updated then (Just hoi_discontinued) else Nothing)
          thisOrder (HouseholdOrderItemData { hoi_orderId, hoi_householdId }) = hoi_orderId == ho_orderId && hoi_householdId == ho_householdId
          items = map item $ filter thisOrder rItems
      in  householdOrder ho_orderId ho_orderCreated ho_orderCreatedBy ho_orderCreatedByName ho_householdId ho_householdName ho_complete ho_cancelled ho_oldTotalExcVat ho_oldTotalIncVat ho_totalExcVat ho_totalIncVat ho_updated items

  getPastHouseholdOrders :: ByteString -> Int -> IO [PastHouseholdOrder]
  getPastHouseholdOrders connectionString groupId = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query conn [sql|
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
      is <- query conn [sql|
        select hoi.order_id, hoi.household_id, hoi.product_id, hoi.product_code, hoi.product_name, hoi.product_price_exc_vat, hoi.product_price_inc_vat, hoi.product_vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat,
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
        order by hoi.household_id, hoi.product_code
      |] (Only groupId)
      return (os :: [PastHouseholdOrderData], is :: [PastHouseholdOrderItemData])
    close conn
    return $ rOrders <&> \(PastHouseholdOrderData { pho_orderId, pho_orderCreated, pho_orderCreatedBy, pho_orderCreatedByName, pho_orderAbandoned, pho_householdId, pho_householdName, pho_cancelled, pho_reconciled, pho_totalExcVat, pho_totalIncVat, pho_oldTotalExcVat, pho_oldTotalIncVat }) ->
      let item (PastHouseholdOrderItemData { phoi_productId, phoi_code, phoi_name, phoi_priceExcVat, phoi_priceIncVat, phoi_vatRate, phoi_quantity, phoi_itemTotalExcVat, phoi_itemTotalIncVat, phoi_b, phoi_f, phoi_g, phoi_o, phoi_s, phoi_v, phoi_oldProductPriceExcVat, phoi_oldProductPriceIncVat, phoi_oldQuantity, phoi_oldItemTotalExcVat, phoi_oldItemTotalIncVat }) 
            = householdOrderItem phoi_productId phoi_code phoi_name phoi_vatRate phoi_priceExcVat phoi_priceIncVat phoi_quantity phoi_itemTotalExcVat phoi_itemTotalIncVat phoi_b phoi_f phoi_g phoi_o phoi_s phoi_v phoi_oldProductPriceExcVat phoi_oldProductPriceIncVat phoi_oldQuantity phoi_oldItemTotalExcVat phoi_oldItemTotalIncVat (Just False)
          thisOrder (PastHouseholdOrderItemData { phoi_orderId, phoi_householdId }) = phoi_orderId == pho_orderId && phoi_householdId == pho_householdId
          items = map item $ filter thisOrder rItems
      in  pastHouseholdOrder pho_orderId pho_orderCreated pho_orderCreatedBy pho_orderCreatedByName pho_orderAbandoned pho_householdId pho_householdName pho_cancelled pho_reconciled pho_totalExcVat pho_totalIncVat pho_oldTotalExcVat pho_oldTotalIncVat items

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
        insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat, product_biodynamic, product_fair_trade, product_gluten_free, product_organic, product_added_sugar, product_vegan)
        select hoi.order_group_id, hoi.order_id, hoi.household_id, hoi.product_id, p.code, p.name, hoi.product_price_exc_Vat, hoi.product_price_inc_vat, p.vat_rate, hoi.quantity, hoi.item_total_exc_vat, hoi.item_total_inc_vat, p.biodynamic, p.fair_trade, p.gluten_free, p.organic, p.added_sugar, p.vegan
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

  ensureHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> String -> UTCTime -> HouseholdOrderItemDetails -> IO ()
  ensureHouseholdOrderItem connectionString groupId orderId householdId productCode date details = do
    let quantity = hoidQuantity details
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
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
            ((Only existingQuantity):_) -> return ()
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
    withTransaction conn $ do
      execute conn [sql|
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
        ((Only existingQuantity):_) -> return ()
        _ -> do
          void $ execute conn [sql|
            insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) values (?, ?, ?, ?, false, false)
          |] (groupId, orderId, householdId, date)
      execute conn [sql|
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
    |]
    close conn
    return $ (rEntries :: [ProductCatalogueEntryData]) <&> \(ProductCatalogueEntryData { pce_code, pce_name, pce_priceExcVat, pce_priceIncVat, pce_vatRate, pce_biodynamic, pce_fairTrade, pce_glutenFree, pce_organic, pce_addedSugar, pce_vegan, pce_category, pce_brand }) -> ProductCatalogueEntry pce_code pce_name pce_priceExcVat pce_priceIncVat pce_vatRate pce_biodynamic pce_fairTrade pce_glutenFree pce_organic pce_addedSugar pce_vegan pce_category pce_brand

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
    execute conn [sql|
      insert into product_image (code, image)
      values (?, ?)
      ON CONFLICT (code) DO UPDATE SET image = EXCLUDED.image;
    |] (code, Binary image)
    close conn

  reconcileOrderItem :: ByteString -> Int -> Int -> Int -> ReconcileOrderItemDetails -> IO ()
  reconcileOrderItem connectionString groupId orderId productId details = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      forM_ (roidHouseholdQuantities details) $ \h -> do
        let price = roidProductPriceExcVat details
        let quantity = hqdItemQuantity h
        let householdId = hqdHouseholdId h

        execute conn [sql|
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

        execute conn [sql|
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