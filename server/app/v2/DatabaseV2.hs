{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DatabaseV2 where

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

data CollectiveOrderData = CollectiveOrderData 
  { order_id :: Int
  , order_created :: UTCTime
  , order_created_by_household_id :: Maybe Int
  , order_created_by_household_name :: Maybe String
  }

instance FromRow CollectiveOrderData where
  fromRow = CollectiveOrderData <$> field <*> field <*> field <*> field

data OrderItemData = OrderItemData 
  { product_id :: Int
  , product_code :: String
  , product_name :: String
  , product_vat_rate :: VatRate
  , product_price :: Int
  , product_biodynamic :: Bool
  , product_fair_trade :: Bool
  , product_gluten_free :: Bool
  , product_organic :: Bool
  , product_added_sugar :: Bool
  , product_vegan :: Bool
  , item_quantity :: Int
  }

instance FromRow OrderItemData where
  fromRow = OrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

getVatRateData :: Connection -> Int -> IO [(VatRate, Rational)]
getVatRateData conn groupId = 
  query_ conn [sql|
    select code, multiplier
    from v2.vat_rate 
  |]

getCollectiveOrderData :: Connection -> Int -> IO [CollectiveOrderData]
getCollectiveOrderData conn groupId = 
  query conn [sql|
    select o.id           as order_id
         , o.created_date as order_created
         , h.id           as order_created_by_household_id
         , h.name         as order_created_by_household_name
    from v2."order" o
    left join v2.household h on h.id = o.created_by_id
    where o.order_group_id = ?
    order by o.id desc
  |] (Only groupId)

getCollectiveOrderItemData :: Connection -> Int -> IO [(Int, OrderItemData)]
getCollectiveOrderItemData conn groupId = do
  items <- query conn [sql|
    select hoi.order_id
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