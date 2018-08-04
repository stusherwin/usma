{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllOrders, getOrderSummary, getHouseholdOrderSummary, createOrder) where
  import Control.Monad (mzero, when)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.Time.Calendar (Day, showGregorian)
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  -- infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  -- infixr 0 &
  
  getAllOrders :: ByteString -> IO [Order]
  getAllOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query_ conn
      " select o.created_date, o.complete, coalesce(sum(p.price * hoi.quantity), 0) as total \
      \ from \"order\" o \
      \ left join household_order ho on ho.order_id = o.id \
      \ left join household_order_item hoi on hoi.order_id = o.id \
      \ left join product p on p.id = hoi.product_id \
      \ group by o.id"
    close conn
    return $ (rOrders :: [(Day, Bool, Int)]) <&> \(createdDate, complete, total) -> Order (showGregorian createdDate) (showGregorian createdDate) complete total

  getOrderSummary :: ByteString -> Day -> IO (Maybe OrderSummary)
  getOrderSummary connectionString day = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query conn
      " select created_date, complete, coalesce(sum(p.price * hoi.quantity), 0) as total \
      \ from \"order\" o\
      \ left join household_order ho on ho.order_id = o.id \
      \ left join household_order_item hoi on hoi.order_id = o.id \
      \ left join product p on p.id = hoi.product_id \
      \ where o.created_date = ? \
      \ group by o.id "
      (Only day)
    rHouseholds <- query conn
      " select h.id, h.name, coalesce(sum(p.price * hoi.quantity), 0) as total, case when ho.status='P' then 'paid' when ho.status='U' then 'unpaid' else 'cancelled' end as status \
      \ from \"order\" o\
      \ inner join household_order ho on ho.order_id = o.id \
      \ inner join household h on h.id = ho.household_id \
      \ left join household_order_item hoi on hoi.household_id = h.id \
      \ left join product p on p.id = hoi.product_id \
      \ where o.created_date = ? \
      \ group by h.id, h.name, ho.status"
      (Only day)
    close conn
    let households = (rHouseholds :: [(Int, String, Int, String)]) <&> \(id, name, total, status) -> OrderSummary_Household id name total status
    return $ listToMaybe $ (rOrders :: [(Day, Bool, Int)]) <&> \(createdDate, complete, total) -> OrderSummary (showGregorian createdDate) (showGregorian createdDate) complete total households

  getHouseholdOrderSummary :: ByteString -> Day -> Int -> IO (Maybe HouseholdOrderSummary)
  getHouseholdOrderSummary connectionString orderCreatedDate householdId = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query conn
      " select o.created_date, h.id, h.\"name\", coalesce(sum(p.price * hoi.quantity), 0) \
      \ from \"order\" o\
      \ inner join household_order ho on ho.order_id = o.id \
      \ inner join household h on h.id = ho.household_id \
      \ left join household_order_item hoi on hoi.order_id = o.id \
      \ left join product p on p.id = hoi.product_id \
      \ where o.created_date = ? and h.id = ? \
      \ group by o.created_date, h.id, h.name "
      (orderCreatedDate, householdId)
    close conn
    return $ listToMaybe $ (rOrders :: [(Day, Int, String, Int)]) <&> \(orderCreatedDate, householdId, householdName, total) -> HouseholdOrderSummary (showGregorian orderCreatedDate) (showGregorian orderCreatedDate) householdId householdName True False total []
  
  createOrder :: ByteString -> Day -> IO ()
  createOrder connectionString date = do
    conn <- connectPostgreSQL connectionString
    execute conn
      "insert into \"order\" (created_date, complete) values (?, false)"
      (Only date)
    close conn
    return ()