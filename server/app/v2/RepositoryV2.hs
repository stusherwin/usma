{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module RepositoryV2 where 

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only(..), Query, (:.)(..), connectPostgreSQL, close, withTransaction, query, query_)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.SqlQQ
import Prelude hiding (sum)

import Config (Config(..))
import DomainV2

getOrderGroupId :: Config -> String -> IO (Maybe OrderGroupId)
getOrderGroupId config groupKey = do
  conn <- connectPostgreSQL $ connectionString config
  result <- query conn [sql|
    select id
    from order_group
    where key = ?
  |] (Only groupKey)
  close conn
  return $ fmap OrderGroupId $ listToMaybe $ fmap fromOnly $ (result :: [Only Int])

getHouseholds :: Config -> OrderGroupId -> IO [Household]
getHouseholds config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rHouseholds, rHouseholdOrders, rHouseholdOrderItems, rPayments) <- withTransaction conn $ do
    v <- selectVatRates conn
    h <- selectHouseholdRows groupId conn
    ho <- selectHouseholdOrderRows groupId Nothing conn
    hoi <- selectHouseholdOrderItemRows groupId Nothing conn
    p <- selectPayments groupId conn
    return (v, h, ho, hoi, p)
  close conn
  return $ map (toHousehold rVatRates rHouseholdOrders rHouseholdOrderItems rPayments) rHouseholds

getOrder :: Config -> OrderGroupId -> IO (Maybe Order)
getOrder config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rOrders, rHouseholdOrders, rHouseholdOrderItems) <- withTransaction conn $ do
    v <- selectVatRates conn
    o <- selectOrderRows groupId (Just OrderIsCurrent) conn
    ho <- selectHouseholdOrderRows groupId (Just OrderIsCurrent) conn
    hoi <- selectHouseholdOrderItemRows groupId (Just OrderIsCurrent) conn
    return (v, o, ho, hoi)
  close conn
  return $ listToMaybe $ map (toOrder rVatRates rHouseholdOrders rHouseholdOrderItems) rOrders
    
getPastOrders :: Config -> OrderGroupId -> IO [Order]
getPastOrders config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rOrders, rHouseholdOrders, rHouseholdOrderItems) <- withTransaction conn $ do
    v <- selectVatRates conn
    o <- selectOrderRows groupId (Just OrderIsPast) conn
    ho <- selectHouseholdOrderRows groupId (Just OrderIsPast) conn
    hoi <- selectHouseholdOrderItemRows groupId (Just OrderIsPast) conn
    return (v, o, ho, hoi)
  close conn
  return $ map (toOrder rVatRates rHouseholdOrders rHouseholdOrderItems) rOrders

toHousehold :: [VatRate] -> [OrderInfo :. HouseholdInfo :. HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow :. OrderItemAdjustmentRow] -> [Payment] -> (HouseholdInfo :. HouseholdRow) -> Household
toHousehold rVatRates rHouseholdOrders rHouseholdOrderItems rPayments (householdInfo :. h) = 
  household householdInfo
            (householdRow_contact_name h)
            (householdRow_contact_email h)
            (householdRow_contact_phone h)
            householdOrders
            rPayments
  where
  -- householdInfo = HouseholdInfo (householdRow_id h) (householdRow_name h)
  householdOrders = map (toHouseholdOrder rVatRates rHouseholdOrderItems)
                  . filter (\(_ :. hi :. _) -> _householdId hi == _householdId householdInfo)
                  $ rHouseholdOrders      

toOrder :: [VatRate] -> [OrderInfo :. HouseholdInfo :. HouseholdOrderRow] -> [(OrderId, HouseholdId) :. OrderItemRow :. OrderItemAdjustmentRow] -> (OrderInfo :. OrderStatusRow) -> Order
toOrder rVatRates rHouseholdOrders rHouseholdOrderItems (orderInfo :. s) = 
  order orderInfo
        (orderRow_is_abandoned s) 
        (orderRow_is_placed s)
        householdOrders
  where
  householdOrders = map (toHouseholdOrder rVatRates rHouseholdOrderItems)
                  . filter (\(oi :. _ :. _) -> _orderId oi == _orderId orderInfo)
                  $ rHouseholdOrders

toHouseholdOrder :: [VatRate] -> [(OrderId, HouseholdId) :. OrderItemRow :. OrderItemAdjustmentRow] -> (OrderInfo :. HouseholdInfo :. HouseholdOrderRow) -> HouseholdOrder
toHouseholdOrder rVatRates rHouseholdOrderItems (orderInfo :. householdInfo :. ho) = 
  householdOrder orderInfo
                 householdInfo
                 (householdOrderRow_is_abandoned ho)
                 (householdOrderRow_is_placed ho)
                 (householdOrderRow_is_complete ho)
                 (householdOrderRow_updated ho)
                 householdOrderItems
  where
  -- householdInfo = HouseholdInfo (householdOrderRow_household_id ho) (householdOrderRow_household_name ho)
  householdOrderItems = map (\(_ :. i :. a) -> toOrderItem rVatRates i a) 
                      . filter (\((orderId, householdId) :. _ :. _) -> orderId == _orderId orderInfo && householdId == _householdId householdInfo)
                      $ rHouseholdOrderItems

toOrderItem :: [VatRate] -> OrderItemRow -> OrderItemAdjustmentRow -> OrderItem
toOrderItem rVatRates i a = 
  orderItem product
            (orderItemRow_quantity i)
            (adjustment a)
  where
  product = Product (orderItemRow_product_id i)
                    (orderItemRow_code i)
                    (orderItemRow_name i)
                    (findVatRate $ orderItemRow_vat_rate i)
                    (atVatRate (findVatRate $ orderItemRow_vat_rate i) $ orderItemRow_price i)
                    (orderItemRow_updated i)
                    attributes
  attributes = ProductAttributes (orderItemRow_biodynamic i)
                                 (orderItemRow_fair_trade i)
                                 (orderItemRow_gluten_free i)
                                 (orderItemRow_organic i)
                                 (orderItemRow_added_sugar i)
                                 (orderItemRow_vegan i)

  adjustment (OrderItemAdjustmentRow (Just new_vat_rate)
                                     (Just new_price)
                                     (Just new_quantity)
                                     (Just is_discontinued)
                                     (Just date))
    = Just $ orderItemAdjustment (findVatRate new_vat_rate)
                                 (atVatRate (findVatRate new_vat_rate) new_price)
                                 new_quantity
                                 is_discontinued
                                 date
  adjustment _ = Nothing
  findVatRate vatRateType = fromMaybe zeroRate $ find ((== vatRateType) . _type) rVatRates

selectVatRates conn = query_ conn [sql| select code, multiplier from v2.vat_rate |]

instance FromRow VatRate where
  fromRow = VatRate <$> field <*> field

instance ToField VatRateType where
  toField Zero = toDatabaseChar 'Z'
  toField Standard = toDatabaseChar 'S'
  toField Reduced = toDatabaseChar 'R'

instance FromField VatRateType where
  fromField f char = do
    c <- fromField f char
    case c of
      Just 'Z' -> return Zero
      Just 'S' -> return Standard
      Just 'R' -> return Reduced
      _ -> mzero

selectHouseholdRows :: OrderGroupId -> Connection -> IO [HouseholdInfo :. HouseholdRow]
selectHouseholdRows groupId conn = 
  query conn ([sql|
    select h.id
         , h.name
         , h.contact_name
         , h.contact_email
         , h.contact_phone
    from household h
    where h.archived = false and h.order_group_id = ?
    order by h.name asc
  |]) (Only $ fromOrderGroupId groupId)

data HouseholdRow = HouseholdRow
  { 
  --   householdRow_id :: HouseholdId
  -- , householdRow_name :: String
  -- , 
    householdRow_contact_name :: Maybe String
  , householdRow_contact_email :: Maybe String
  , householdRow_contact_phone :: Maybe String
  }

instance FromRow HouseholdRow where
  fromRow = HouseholdRow <$> field <*> field <*> field -- <*> field <*> field

selectPayments :: OrderGroupId -> Connection -> IO [Payment]
selectPayments groupId conn = 
  query conn [sql|
    select p.id
         , p.household_id
         , p."date"
         , p.amount
    from household_payment p
    where p.archived = false and p.order_group_id = ?
    order by p.id asc
  |] (Only $ fromOrderGroupId groupId)

instance FromRow Payment where
  fromRow = Payment <$> field <*> field <*> field <*> field

selectOrderRows :: OrderGroupId -> Maybe OrderWhereCondition -> Connection -> IO [OrderInfo :. OrderStatusRow]
selectOrderRows groupId whereCondition conn = 
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
    where o.order_group_id = ?
  |] <> case whereCondition of 
       Just OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
       Just OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |]
       _ -> mempty
     <> [sql|
    order by o.id desc
  |]) (Only $ fromOrderGroupId groupId)

data OrderStatusRow = OrderStatusRow
  { 
    -- orderRow_id :: OrderId
  -- , orderRow_created :: UTCTime
  -- , orderRow_created_by_household_id :: Maybe HouseholdId
  -- , orderRow_created_by_household_name :: Maybe String
  --,
    orderRow_is_abandoned :: Bool
  , orderRow_is_placed :: Bool
  }

instance FromRow OrderStatusRow where
  fromRow = OrderStatusRow <$> field <*> field -- <*> field <*> field <*> field <*> field

selectHouseholdOrderRows :: OrderGroupId -> Maybe OrderWhereCondition -> Connection -> IO [OrderInfo :. HouseholdInfo :. HouseholdOrderRow]
selectHouseholdOrderRows groupId whereCondition conn = 
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
  left join household_order_item hoi 
    on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
  left join product p 
    on p.id = hoi.product_id
  where o.order_group_id = ?
  |] <> case whereCondition of 
       Just OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
       Just OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |]
       _ -> mempty
     <> [sql|
  order by o.id desc, h.name asc
|]) (Only $ fromOrderGroupId groupId)

data HouseholdOrderRow = HouseholdOrderRow 
  { 
  --   householdOrderRow_household_id :: HouseholdId
  -- , householdOrderRow_household_name :: String
  -- ,
    householdOrderRow_is_abandoned :: Bool
  , householdOrderRow_is_placed :: Bool
  , householdOrderRow_is_complete :: Bool
  , householdOrderRow_updated :: UTCTime
  }

instance FromRow HouseholdOrderRow where
  fromRow = HouseholdOrderRow <$> field <*> field <*> field <*> field -- <*> field <*> field

instance FromRow OrderInfo where
  fromRow = do
    orderId  <- field
    created  <- field
    createdById <- field
    createdByName  <- field
    return $ OrderInfo orderId created (createdBy createdById createdByName)
    where
    createdBy (Just id) (Just name) = Just $ HouseholdInfo id name
    createdBy _ _                   = Nothing

instance FromRow HouseholdInfo where
  fromRow = HouseholdInfo <$> field <*> field

selectHouseholdOrderItemRows :: OrderGroupId -> Maybe OrderWhereCondition -> Connection -> IO [(OrderId, HouseholdId) :. OrderItemRow :. OrderItemAdjustmentRow]
selectHouseholdOrderItemRows groupId whereCondition conn = 
  query conn ([sql|
    select hoi.order_id
         , hoi.household_id
         , p.id
         , p.code
         , p.name
         , p.vat_rate
         , p.price
         , p.biodynamic
         , p.fair_trade
         , p.gluten_free
         , p.organic
         , p.added_sugar
         , p.vegan
         , p.updated
         , hoi.quantity
         , adj.new_vat_rate
         , adj.new_price
         , adj.new_quantity
         , adj.date
    from v2.household_order_item hoi
    inner join v2.household_order ho 
      on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join v2."order" o
      on ho.order_id = o.id
    inner join v2.product p 
      on p.id = hoi.product_id
    let join v2.order_item_adjustment adj
      on adj.order_id = hoi.order_id and adj.household_id = hoi.household_id and adj.product_id = hoi.product_id
    where o.order_group_id = ? 
  |] <> case whereCondition of 
       Just OrderIsCurrent -> [sql| and (o.is_abandoned = 'f' and o.is_placed = 'f') |]
       Just OrderIsPast    -> [sql| and (o.is_abandoned = 't' or o.is_placed = 't') |]
       _ -> mempty
     <> [sql|
    order by hoi.ix
  |]) (Only $ fromOrderGroupId groupId)

data OrderItemRow = OrderItemRow 
  { orderItemRow_product_id :: ProductId
  , orderItemRow_code :: String
  , orderItemRow_name :: String
  , orderItemRow_vat_rate :: VatRateType
  , orderItemRow_price :: Int
  , orderItemRow_biodynamic :: Bool
  , orderItemRow_fair_trade :: Bool
  , orderItemRow_gluten_free :: Bool
  , orderItemRow_organic :: Bool
  , orderItemRow_added_sugar :: Bool
  , orderItemRow_vegan :: Bool
  , orderItemRow_updated :: UTCTime
  , orderItemRow_quantity :: Int
  }

instance FromRow OrderItemRow where
  fromRow = OrderItemRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data OrderItemAdjustmentRow = OrderItemAdjustmentRow 
  { orderItemAdjRow_new_vat_rate :: Maybe VatRateType
  , orderItemAdjRow_new_price :: Maybe Int
  , orderItemAdjRow_new_quantity :: Maybe Int
  , orderItemAdjRow_is_discontinued :: Maybe Bool
  , orderItemAdjRow_date :: Maybe UTCTime
  }

instance FromRow OrderItemAdjustmentRow where
  fromRow = OrderItemAdjustmentRow <$> field <*> field <*> field <*> field <*> field

data OrderWhereCondition = OrderIsCurrent
                         | OrderIsPast

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

instance FromField HouseholdId where
  fromField f char = HouseholdId <$> fromField f char

instance FromField ProductId where
  fromField f char = ProductId <$> fromField f char

instance FromField OrderId where
  fromField f char = OrderId <$> fromField f char

instance FromField PaymentId where
  fromField f char = PaymentId <$> fromField f char