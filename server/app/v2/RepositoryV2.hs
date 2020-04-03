{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module RepositoryV2 where 

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only(..), (:.)(..), connectPostgreSQL, close, withTransaction, query, query_)
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

getOrder :: Config -> OrderGroupId -> IO (Maybe Order)
getOrder config groupId = do
  let g = fromOrderGroupId groupId
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rOrders, rHouseholdOrders, rHouseholdOrderItems) <- withTransaction conn $ do
    v <- getVatRateRows conn g
    o <- getOrderRows conn g
    ho <- getHouseholdOrderRows conn g
    hoi <- getHouseholdOrderItemRows conn g
    return (v, o, ho, hoi)
  close conn
  return $ listToMaybe $ transform rVatRates rOrders rHouseholdOrders rHouseholdOrderItems
  where
  transform rVatRates rOrders rHouseholdOrders rHouseholdOrderItems = map order rOrders
    where
    order o = DomainV2.order orderInfo
                             (orderRow_is_abandoned o) 
                             (orderRow_is_placed o)
                             householdOrders
      where
      orderId = orderRow_id o
      
      orderInfo = OrderInfo (OrderId orderId)
                            (orderRow_created o) 
                            $ createdBy (orderRow_created_by_household_id o) (orderRow_created_by_household_name o)
      
      createdBy (Just id) (Just name) = Just $ HouseholdInfo (HouseholdId id) name
      createdBy _ _                   = Nothing

      orderItem (i, a) = DomainV2.orderItem product
                                            (orderItemRow_quantity i)
                                            (adjustment a)
        where
        product = Product (ProductId . orderItemRow_product_id $ i)
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
      
        adjustment (OrderItemAdjustmentRow { orderItemAdjRow_new_vat_rate = Just new_vat_rate
                                           , orderItemAdjRow_new_price = Just new_price
                                           , orderItemAdjRow_new_quantity = Just new_quantity
                                           , orderItemAdjRow_is_discontinued = Just is_discontinued
                                           , orderItemAdjRow_date = Just date
                                           })
          = Just $ orderItemAdjustment (findVatRate new_vat_rate)
                                       (atVatRate (findVatRate new_vat_rate) new_price)
                                       new_quantity
                                       is_discontinued
                                       date
        adjustment _ = Nothing

      householdOrders = map (householdOrder . snd) 
                      . filter ((orderId ==) . fst) 
                      $ rHouseholdOrders      
      householdOrder ho = DomainV2.householdOrder orderInfo
                                                  householdInfo
                                                  (householdOrderRow_is_abandoned ho)
                                                  (householdOrderRow_is_placed ho)
                                                  (householdOrderRow_is_complete ho)
                                                  (householdOrderRow_updated ho)
                                                  householdOrderItems
        where
        householdId = householdOrderRow_household_id $ ho
        householdInfo = HouseholdInfo (HouseholdId householdId) (householdOrderRow_household_name ho)
        householdOrderItems = map (orderItem . snd) 
                            . filter ((\(oId, hId) -> oId == orderId && hId == householdId) . fst) 
                            $ rHouseholdOrderItems
      findVatRate vatRateType = fromMaybe zeroRate $ find ((== vatRateType) . _type) rVatRates
          
getVatRateRows :: Connection -> Int -> IO [VatRate]
getVatRateRows conn groupId = 
  query_ conn [sql|
    select code, multiplier
    from v2.vat_rate 
  |]

data OrderRow = OrderRow
  { orderRow_id :: Int
  , orderRow_created :: UTCTime
  , orderRow_created_by_household_id :: Maybe Int
  , orderRow_created_by_household_name :: Maybe String
  , orderRow_is_abandoned :: Bool
  , orderRow_is_placed :: Bool
  }

instance FromRow OrderRow where
  fromRow = OrderRow <$> field <*> field <*> field <*> field <*> field <*> field

getOrderRows :: Connection -> Int -> IO [OrderRow]
getOrderRows conn groupId = 
  query conn [sql|
    select o.id
         , o.created
         , h.id as created_by_household_id
         , h.name as created_by_household_name
         , o.is_abandoned
         , o.is_placed
    from v2."order" o
    left join v2.household h 
      on h.id = o.created_by_id
    where o.order_group_id = ?
    order by o.id desc
  |] (Only groupId)

data HouseholdOrderRow = HouseholdOrderRow 
  { householdOrderRow_household_id :: Int
  , householdOrderRow_household_name :: String
  , householdOrderRow_is_abandoned :: Bool
  , householdOrderRow_is_placed :: Bool
  , householdOrderRow_is_complete :: Bool
  , householdOrderRow_updated :: UTCTime
  }

instance FromRow HouseholdOrderRow where
  fromRow = HouseholdOrderRow <$> field <*> field <*> field <*> field <*> field <*> field

getHouseholdOrderRows :: Connection -> Int -> IO [(Int, HouseholdOrderRow)]
getHouseholdOrderRows conn groupId = do
  rows <- query conn [sql|
    select o.id as order_id

         , h.id as household_id
         , h.name as household_name
         , ho.is_abandoned
         , o.is_placed
         , ho.is_complete
         , ho.updated
    from v2.household_order ho
    inner join v2."order" o 
      on o.id = ho.order_id
    inner join v2.household h 
      on h.id = ho.household_id
    left join household_order_item hoi 
      on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
    left join product p 
      on p.id = hoi.product_id
    where o.order_group_id = ?
    order by o.id desc, h.name asc
  |] (Only groupId)
  return $ map (\((Only orderId) :. orderRow) -> (orderId, orderRow)) rows

data OrderItemRow = OrderItemRow 
  { orderItemRow_product_id :: Int
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

getHouseholdOrderItemRows :: Connection -> Int -> IO [((Int, Int), (OrderItemRow, OrderItemAdjustmentRow))]
getHouseholdOrderItemRows conn groupId = do
  rows <- query conn [sql|
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
        --  , adj.new_product_price_inc_vat
         , adj.new_quantity
        --  , adj.new_item_total_exc_vat
        --  , adj.new_item_total_inc_vat
         , adj.date
    from v2.household_order_item hoi
    inner join v2.household_order ho 
      on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join v2.product p 
      on p.id = hoi.product_id
    let join v2.order_item_adjustment adj
      on adj.order_id = hoi.order_id and adj.household_id = hoi.household_id and adj.product_id = hoi.product_id
    where ho.order_group_id = ?
    order by hoi.ix
  |] (Only groupId)
  return $ map (\((orderId, householdId) :. itemRow :. itemAdjRow) -> ((orderId, householdId), (itemRow, itemAdjRow))) rows

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

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]