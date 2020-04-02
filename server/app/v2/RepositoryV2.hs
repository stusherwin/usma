{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module RepositoryV2 where

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

import DomainV2
import DatabaseV2
import Data.Maybe (listToMaybe)
import Config
import Prelude hiding (sum)

getOrder :: Config -> Int -> IO (Maybe Order)
getOrder config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rOrders, rItems) <- withTransaction conn $ do
    v <- getVatRateRows conn groupId
    o <- getOrderRows conn groupId
    i <- getOrderItemRows conn groupId
    return (v, o, i)
  close conn
  return $ listToMaybe $ transform rVatRates rOrders rItems
  where
  transform rVatRates rOrders rItems = map order rOrders
    where
    order o = DomainV2.order (orderInfo o)
                             (orderRow_is_placed o)
                             (orderRow_is_abandoned o) 
                             items
                             undefined
      where
      
      items = map (item . snd) $ filter ((orderRow_id o == ) . fst) rItems

      orderInfo o = OrderInfo (OrderId . orderRow_id $ o) 
                              (orderRow_created o) 
                              $ householdInfo (orderRow_created_by_household_id o) (orderRow_created_by_household_name o)
      
      householdInfo (Just id) (Just name) = Just $ HouseholdInfo (HouseholdId id) name
      householdInfo _ _                   = Nothing
            
      item i = OrderItem (product i)
                         (orderItemRow_quantity i)
                         (atProductVatRate i $ orderItemRow_price i * orderItemRow_quantity i)
                         undefined
      
      product i = Product (ProductId . orderItemRow_product_id $ i)
                          (orderItemRow_code i)
                          (orderItemRow_name i)
                          (orderItemRow_vat_rate i)
                          (atProductVatRate i $ orderItemRow_price i)
                          (attributes i)
      
      attributes i = ProductAttributes (orderItemRow_biodynamic i)
                                       (orderItemRow_fair_trade i)
                                       (orderItemRow_gluten_free i)
                                       (orderItemRow_organic i)
                                       (orderItemRow_added_sugar i)
                                       (orderItemRow_vegan i)

      atProductVatRate i = atVatRate rVatRates (orderItemRow_vat_rate i)

getVatRateRows :: Connection -> Int -> IO VatRates
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
  , orderRow_is_placed :: Bool
  , orderRow_is_abandoned :: Bool
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
         , o.is_placed
         , o.is_abandoned
    from v2."order" o
    left join v2.household h on h.id = o.created_by_id
    where o.order_group_id = ?
    order by o.id desc
  |] (Only groupId)

data OrderItemRow = OrderItemRow 
  { orderItemRow_product_id :: Int
  , orderItemRow_code :: String
  , orderItemRow_name :: String
  , orderItemRow_vat_rate :: VatRate
  , orderItemRow_price :: Int
  , orderItemRow_biodynamic :: Bool
  , orderItemRow_fair_trade :: Bool
  , orderItemRow_gluten_free :: Bool
  , orderItemRow_organic :: Bool
  , orderItemRow_added_sugar :: Bool
  , orderItemRow_vegan :: Bool
  , orderItemRow_quantity :: Int
  }

instance FromRow OrderItemRow where
  fromRow = OrderItemRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

getOrderItemRows :: Connection -> Int -> IO [(Int, OrderItemRow)]
getOrderItemRows conn groupId = do
  items <- query conn [sql|
    select hoi.order_id
         , p.id as product_id
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
         , sum(hoi.quantity) as quantity
    from v2.household_order_item hoi
    inner join v2.household_order ho on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join v2.product p on p.id = hoi.product_id
    inner join v2.vat_rate v on p.vat_rate = v.code
    where ho.order_group_id = ? and not ho.cancelled
    group by hoi.order_id
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
    order by p.code asc
  |] (Only groupId)
  return $ map (\((Only id) :. d) -> (id, d)) items

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

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]