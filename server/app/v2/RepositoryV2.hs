{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepositoryV2 where 

import Control.Applicative ((<|>))
import Control.Monad (mzero, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.List (find, foldl')
import Data.Maybe (listToMaybe, fromMaybe)
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
data Repository = Repository { connection :: Connection, orderGroupId :: OrderGroupId }

connect :: RepositoryConfig -> (Repository -> MaybeT IO a) -> MaybeT IO a
connect config action = do
  conn <- liftIO $ connectPostgreSQL $ repoConnectionString $ config
  (maybeGroupId :: [Only Int]) <- liftIO $ query conn [sql|
    select id
    from order_group
    where key = ?
  |] (Only $ repoGroupKey config)
  connection <- MaybeT . return $ Repository conn . OrderGroupId . fromOnly <$> listToMaybe maybeGroupId
  result <- MaybeT $ liftIO $ withTransaction conn $ runMaybeT $ action connection
  liftIO $ close conn
  return result

getHouseholds :: Repository -> IO [Household]
getHouseholds repo = do
  let conn = connection repo
  let groupId = orderGroupId repo

  (rHouseholds, rHouseholdOrders, rOrderItems, rPayments) <- do
    rHouseholds <- selectHouseholdRows conn [ForOrderGroup groupId]
    rHouseholdOrders <- selectHouseholdOrderRows conn [ForOrderGroup groupId]
    rOrderItems <- selectHouseholdOrderItemRows conn [ForOrderGroup groupId]
    rPayments <- selectPayments conn [ForOrderGroup groupId]
    return (rHouseholds, rHouseholdOrders, rOrderItems, rPayments)
  return $ map (toHousehold rHouseholdOrders rOrderItems rPayments) rHouseholds

createProduct :: Repository -> String -> IO (Maybe Product)
createProduct repo code = do
  let conn = connection repo

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
    where ce.code = ?
    on conflict ("code") do nothing
  |] (Only code)
  listToMaybe <$> selectProducts conn [ForProduct code]

getOrder :: Repository -> IO (Maybe Order)
getOrder repo = do
  let conn = connection repo
  let groupId = orderGroupId repo

  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn [OrderIsCurrent, ForOrderGroup groupId]
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent, ForOrderGroup groupId]
    rOrderItems <- selectHouseholdOrderItemRows  conn [OrderIsCurrent, ForOrderGroup groupId]
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getOrdersForAllGroups :: Repository -> IO [Order]
getOrdersForAllGroups repo = do
  let conn = connection repo
  let groupId = orderGroupId repo

  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn [OrderIsCurrent]
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent]
    rOrderItems <- selectHouseholdOrderItemRows  conn [OrderIsCurrent]
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getPastOrders :: Repository -> IO [Order]
getPastOrders repo = do
  let conn = connection repo
  let groupId = orderGroupId repo
  
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn [OrderIsPast, ForOrderGroup groupId]
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsPast, ForOrderGroup groupId]
    rOrderItems <- selectHouseholdOrderItemRows  conn [OrderIsPast, ForOrderGroup groupId]
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

createHouseholdOrder :: Repository -> OrderId -> HouseholdId -> UTCTime -> IO (Maybe HouseholdOrder)
createHouseholdOrder repo orderId householdId date = do
  let conn = connection repo
  let groupId = orderGroupId repo

  execute conn [sql|
    insert into household_order (order_group_id, order_id, household_id, updated, complete, cancelled) 
    values (?, ?, ?, ?, false, false)
    on conflict (order_group_id, order_id, household_id) do nothing
  |] (groupId, orderId, householdId, date)

  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent, ForOrderGroup groupId, ForOrder orderId, ForHousehold householdId]
    rOrderItems <- selectHouseholdOrderItemRows  conn  [OrderIsCurrent, ForOrderGroup groupId, ForOrder orderId, ForHousehold householdId]
    return (rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

getHouseholdOrder :: Repository -> OrderId -> HouseholdId -> IO (Maybe HouseholdOrder)
getHouseholdOrder repo orderId householdId = do
  let conn = connection repo
  let groupId = orderGroupId repo
  
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent, ForOrderGroup groupId, ForOrder orderId, ForHousehold householdId]
    rOrderItems <- selectHouseholdOrderItemRows  conn [OrderIsCurrent, ForOrderGroup groupId, ForOrder orderId, ForHousehold householdId]
    return (rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

newOrder :: Repository -> OrderSpec -> IO OrderId
newOrder repo spec = do
  let conn = connection repo
  let groupId = orderGroupId repo
  
  [Only id] <- query conn [sql|
    insert into "order" (order_group_id, created_date, created_by_id) 
    values (?, ?, ?) 
    returning id
  |] (groupId, _orderSpecCreated spec, _orderSpecCreatedByHouseholdId spec)
  return id



updateHouseholdOrder :: Repository -> HouseholdOrder -> IO ()
updateHouseholdOrder repo order = do
  let conn = connection repo
  let groupId = orderGroupId repo
  
  let orderId = _orderId . _householdOrderOrderInfo $ order
  let householdId = _householdId . _householdOrderHouseholdInfo $ order
  let abandoned = isHouseholdOrderAbandoned order
  let complete = isHouseholdOrderComplete order

  execute conn [sql|
    update household_order 
    set cancelled = ? 
      , complete = ?
    where order_group_id = ? and order_id = ? and household_id = ?
  |] (abandoned, complete, groupId, orderId, householdId)

  forM_ (_householdOrderItems order) $ \i -> do
    let productInfo = _productInfo . _itemProduct $ i
    let productId = fromProductId . _productId $ productInfo
    let productPriceExcVat = _moneyExcVat . _priceAmount . _productPrice $ productInfo
    let productPriceIncVat = _moneyIncVat . _priceAmount . _productPrice $ productInfo
    let quantity = _itemQuantity i
    void $ execute conn [sql|
      insert into household_order_item as hoi 
        ( order_group_id
        , order_id
        , household_id
        , product_id
        , product_price_exc_vat
        , product_price_inc_vat
        , quantity
        )
      values (?, ?, ?, ?, ?, ?, ?)
      on conflict (order_group_id, order_id, household_id, product_id) do update
      set quantity = hoi.quantity
    |] (groupId, orderId, householdId, productId, productPriceExcVat, productPriceIncVat, quantity)

updateProductCatalogue :: Repository -> UTCTime -> ProductCatalogue -> IO [Product]
updateProductCatalogue repo date entries = do
  let conn = connection repo
  execute_ conn [sql|
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
  selectProducts conn []

updateOrders :: Repository -> [Order] -> IO ()
updateOrders repo orders = return ()

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
           , o.created
           , cb.id as created_by_household_id
           , cb.name as created_by_household_name
           , o.is_abandoned
           , o.is_placed
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

selectHouseholdOrderItemRows :: Connection -> [WhereParam] -> IO [(OrderId, HouseholdId) :. OrderItemRow]
selectHouseholdOrderItemRows conn whereParams = 
    query conn ([sql|
      select hoi.order_id
           , hoi.household_id
           , p.id
           , p.code
           , p.name
           , p.vat_rate
           , v.multiplier
           , p.price
           , p.updated
           , p.biodynamic
           , p.fair_trade
           , p.gluten_free
           , p.organic
           , p.added_sugar
           , p.vegan
           , hoi.quantity
           , adj.new_vat_rate
           , adjv.multiplier
           , adj.new_price
           , adj.new_quantity
           , p.discontinued
           , adj.date
      from v2.household_order_item hoi
      inner join v2.household_order ho 
        on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
      inner join v2."order" o
        on ho.order_id = o.id
      inner join v2.product p 
        on p.id = hoi.product_id
      inner join v2.vat_rate v
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

toHousehold :: [HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow] -> [Payment] -> (HouseholdRow) -> Household
toHousehold rHouseholdOrders rOrderItems rPayments h = 
  household householdInfo
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
  order orderInfo
        (orderRow_statusFlags o)
        householdOrders
  where
  orderInfo = orderRow_orderInfo o
  householdOrders = map (toHouseholdOrder rOrderItems)
                  . filter ((== _orderId orderInfo) . _orderId . householdOrderRow_orderInfo)
                  $ rHouseholdOrders

toHouseholdOrder :: [(OrderId, HouseholdId) :. OrderItemRow] -> HouseholdOrderRow -> HouseholdOrder
toHouseholdOrder rOrderItems ho = 
  householdOrder orderInfo
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
toOrderItem (_ :. i) = orderItem (orderItemRow_product i)
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
    created  <- field
    createdById <- field
    createdByName  <- field
    return $ OrderInfo orderId created (createdBy createdById createdByName)
    where
    createdBy (Just id) (Just name) = Just $ HouseholdInfo id name
    createdBy _ _                   = Nothing

instance FromField OrderId where
  fromField f char = OrderId <$> fromField f char

instance ToField OrderId where
  toField = toField . fromOrderId

orderStatusFlagsField :: RowParser OrderStatusFlags
orderStatusFlagsField = OrderStatusFlags <$> field <*> field

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
productInfoField = ProductInfo <$> field <*> field <*> field <*> priceField <*> field

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
    (Just p, Just q, Just disc, Just date) -> return $ Just $ orderItemAdjustment p q disc date
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