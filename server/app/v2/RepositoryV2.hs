{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepositoryV2 where 

import Control.Applicative ((<|>))
import Control.Monad (mzero)
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
import Database.PostgreSQL.Simple (Connection, Only(..), Query, (:.)(..), connectPostgreSQL, close, withTransaction, query, query_)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), RowParser, field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.SqlQQ
import Prelude hiding (sum)

import Config (Config(..))
import DomainV2

data RepositoryConfig = RepositoryConfig { repoConnectionString :: ByteString, repoGroupKey :: String }
data Repository = Repository { connection :: Connection, groupId :: OrderGroupId }

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
getHouseholds conn = do
  (rHouseholds, rHouseholdOrders, rOrderItems, rPayments) <- do
    rHouseholds <- selectHouseholdRows conn
    rHouseholdOrders <- selectHouseholdOrderRows conn []
    rOrderItems <- selectHouseholdOrderItemRows conn []
    rPayments <- selectPayments conn
    return (rHouseholds, rHouseholdOrders, rOrderItems, rPayments)
  return $ map (toHousehold rHouseholdOrders rOrderItems rPayments) rHouseholds

getOrder :: Repository -> IO (Maybe Order)
getOrder conn = do
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn [OrderIsCurrent]
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent]
    rOrderItems <- selectHouseholdOrderItemRows conn [OrderIsCurrent]
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toOrder rHouseholdOrders rOrderItems) rOrders
    
getPastOrders :: Repository -> IO [Order]
getPastOrders conn = do
  (rOrders, rHouseholdOrders, rOrderItems) <- do
    rOrders <- selectOrderRows conn [OrderIsPast]
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsPast]
    rOrderItems <- selectHouseholdOrderItemRows conn [OrderIsPast]
    return (rOrders, rHouseholdOrders, rOrderItems)
  return $ map (toOrder rHouseholdOrders rOrderItems) rOrders

getHouseholdOrder :: Repository -> OrderId -> HouseholdId -> IO (Maybe HouseholdOrder)
getHouseholdOrder conn orderId householdId = do
  (rHouseholdOrders, rOrderItems) <- do
    rHouseholdOrders <- selectHouseholdOrderRows conn [OrderIsCurrent]
    rOrderItems <- selectHouseholdOrderItemRows conn [OrderIsCurrent]
    return (rHouseholdOrders, rOrderItems)
  return $ listToMaybe $ map (toHouseholdOrder rOrderItems) rHouseholdOrders

newOrder :: Repository -> OrderSpec -> IO OrderId
newOrder conn spec = do
  [Only id] <- query (connection conn) [sql|
    insert into "order" (order_group_id, created_date, created_by_id) values (?, ?, ?) returning id
  |] (groupId conn, _orderSpecCreated spec, _orderSpecCreatedByHouseholdId spec)
  return id

selectHouseholdRows :: Repository -> IO [HouseholdRow]
selectHouseholdRows conn = 
  query (connection conn) ([sql|
    select h.id
         , h.name
         , h.contact_name
         , h.contact_email
         , h.contact_phone
    from household h
    where h.archived = false and h.order_group_id = ?
    order by h.name asc
  |]) (Only $ groupId conn)

selectPayments :: Repository -> IO [Payment]
selectPayments conn = 
  query (connection conn) [sql|
    select p.id
         , p.household_id
         , p."date"
         , p.amount
    from household_payment p
    where p.archived = false and p.order_group_id = ?
    order by p.id asc
  |] (Only $ groupId conn)

selectOrderRows :: Repository -> [WhereCondition] -> IO [OrderRow]
selectOrderRows conn whereCondition = 
  query (connection conn) ([sql|
    select o.id
         , o.created
         , cb.id as created_by_household_id
         , cb.name as created_by_household_name
         , o.is_abandoned
         , o.is_placed
    from v2."order" o
    left join v2.household cb
      on cb.id = o.created_by_id 
    where o.order_group_id = ?
  |] <> mapWhere whereCondition (\w -> case w of 
          OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
          OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |])
     <> [sql|
    order by o.id desc
  |]) (Only $ groupId conn)

selectHouseholdOrderRows :: Repository -> [WhereCondition] -> IO [HouseholdOrderRow]
selectHouseholdOrderRows conn whereCondition = 
  query (connection conn) ([sql|
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
  left join household_order_item hoi 
    on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
  left join product p 
    on p.id = hoi.product_id
  where o.order_group_id = ?
  |] <> mapWhere whereCondition (\w -> case w of 
          OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
          OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |])
     <> [sql|
  order by o.id desc, h.name asc
|]) (Only $ groupId conn)

selectHouseholdOrderItemRows :: Repository -> [WhereCondition] -> IO [(OrderId, HouseholdId) :. OrderItemRow]
selectHouseholdOrderItemRows repo whereCondition = 
  query (connection repo) ([sql|
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
    where o.order_group_id = ? 
  |] <> mapWhere whereCondition (\w -> case w of 
          OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
          OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |])
     <> [sql|
    order by hoi.ix
  |]) (Only $ groupId repo)

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

data WhereCondition = OrderIsCurrent
                    | OrderIsPast

mapWhere :: [WhereCondition] -> (WhereCondition -> Query) -> Query
mapWhere whereCondition = foldl' (<>) mempty . (flip map) whereCondition

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]