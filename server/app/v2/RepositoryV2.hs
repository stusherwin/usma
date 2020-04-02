{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module RepositoryV2 where 

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
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
                             (orderRow_is_placed o)
                             (orderRow_is_abandoned o) 
                             householdOrders
      where
      orderId = orderRow_id o
      
      orderInfo = OrderInfo (OrderId orderId)
                            (orderRow_created o) 
                            $ createdBy (orderRow_created_by_household_id o) (orderRow_created_by_household_name o)
      
      createdBy (Just id) (Just name) = Just $ HouseholdInfo (HouseholdId id) name
      createdBy _ _                   = Nothing

      orderItem i = OrderItem product
                         (orderItemRow_quantity i)
                         (atProductVatRate $ orderItemRow_price i * orderItemRow_quantity i)
                         adjustment
        where
        product = Product (ProductId . orderItemRow_product_id $ i)
                          (orderItemRow_code i)
                          (orderItemRow_name i)
                          (orderItemRow_vat_rate i)
                          (atProductVatRate $ orderItemRow_price i)
                          (orderItemRow_updated i)
                          attributes
        attributes = ProductAttributes (orderItemRow_biodynamic i)
                                       (orderItemRow_fair_trade i)
                                       (orderItemRow_gluten_free i)
                                       (orderItemRow_organic i)
                                       (orderItemRow_added_sugar i)
                                       (orderItemRow_vegan i)
      
        atProductVatRate = atVatRate rVatRates $ orderItemRow_vat_rate i
        adjustment = undefined

      householdOrders = map (householdOrder . snd) . filter ((orderId ==) . fst) $ rHouseholdOrders      
      householdOrder ho = DomainV2.householdOrder orderInfo
                                                  householdInfo
                                                  adjustment
                                                  householdOrderItems
        where
        householdId = householdOrderRow_household_id $ ho
        householdInfo = HouseholdInfo (HouseholdId householdId) (householdOrderRow_household_name ho)
        adjustment = undefined
        householdOrderItems = map (orderItem . snd) . filter ((\(oId, hId) -> oId == orderId && hId == householdId) . fst) $ rHouseholdOrderItems
          
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
    left join v2.household h 
      on h.id = o.created_by_id
    where o.order_group_id = ?
    order by o.id desc
  |] (Only groupId)

data HouseholdOrderRow = HouseholdOrderRow 
  { householdOrderRow_household_id :: Int
  , householdOrderRow_household_name :: String
  , householdOrderRow_is_complete :: Bool
  , householdOrderRow_is_abandoned :: Bool
  }

instance FromRow HouseholdOrderRow where
  fromRow = HouseholdOrderRow <$> field <*> field <*> field <*> field

getHouseholdOrderRows :: Connection -> Int -> IO [(Int, HouseholdOrderRow)]
getHouseholdOrderRows conn groupId = do
  rows <- query conn [sql|
    select o.id as order_id

         , h.id as household_id
         , h.name as household_name
         , ho.is_complete
         , ho.is_cancelled
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
  , orderItemRow_vat_rate :: VatRate
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

getHouseholdOrderItemRows :: Connection -> Int -> IO [((Int, Int), OrderItemRow)]
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
    from v2.household_order_item hoi
    inner join v2.household_order ho 
      on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join v2.product p 
      on p.id = hoi.product_id
    where ho.order_group_id = ?
    order by hoi.ix
  |] (Only groupId)
  return $ map (\((orderId, householdId) :. itemRow) -> ((orderId, householdId), itemRow)) rows

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