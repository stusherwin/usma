{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Database ( getCollectiveOrders, getHouseholdOrders, getProducts, getHouseholds, getHouseholdPayments
                , createOrder, ensureHouseholdOrderItem, removeHouseholdOrderItem, cancelHouseholdOrder
                , uncancelHouseholdOrder, addHouseholdOrder, createHousehold, archiveHousehold
                , createProduct, archiveProduct
                ) where
  import Control.Monad (mzero, when, void)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Database.PostgreSQL.Simple.SqlQQ
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.Time.Calendar (Day, showGregorian)
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  infixr 0 &
  
  getCollectiveOrders :: ByteString -> IO [CollectiveOrder]
  getCollectiveOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        with orders as (
               select o.id, o.created_date, o.complete, coalesce(bool_and(ho.cancelled), false) as cancelled
               from "order" o
               left join household_order ho on ho.order_id = o.id
               group by o.id, o.created_date, o.complete
               order by o.id desc
             ),
             orders_past as (
               (select id, created_date, complete, cancelled, false as past
               from orders
               limit 1)
               union
               (select id, created_date, complete, cancelled, true as past
               from orders
               offset 1)
             )
        select o.id, o.created_date, o.complete, o.cancelled, o.past, coalesce(sum(p.price * hoi.quantity), 0) as total
        from orders_past o
        left join household_order ho on ho.order_id = o.id and ho.cancelled = false 
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        group by o.id, o.created_date, o.complete, o.cancelled, o.past
        order by o.id desc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, p.id, p.name, sum(hoi.quantity) as quantity, sum(p.price * hoi.quantity) as total
        from household_order_item hoi
        inner join product p on p.id = hoi.product_id
        group by hoi.order_id, p.id, p.name
        order by p.name asc
      |]
      return (os :: [(Int, Day, Bool, Bool, Bool, Int)], is :: [(Int, Int, String, Int, Int)])
    close conn
    return $ rOrders <&> \(id, created, complete, cancelled, past, total) ->
      let item (_, productId, name, quantity, total) = CollectiveOrderItem productId name quantity total
          thisOrder (oId, _, _, _, _) = oId == id
          items = map item $ filter thisOrder rItems
      in  CollectiveOrder id created complete cancelled past total items
  
  getHouseholdOrders :: ByteString -> IO [HouseholdOrder]
  getHouseholdOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        with orders as (
               select o.id, o.created_date, o.complete
               from "order" o
               order by o.id desc
             ),
             orders_past as (
               (select id, created_date, complete, false as past
               from orders
               limit 1)
               union
               (select id, created_date, complete, true as past
               from orders
               offset 1)
             )
        select o.id, o.created_date, o.complete, o.past, h.id, h.name, ho.cancelled, coalesce(sum(p.price * hoi.quantity), 0) as total
        from household_order ho
        inner join orders_past o on o.id = ho.order_id
        inner join household h on h.id = ho.household_id
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        group by o.id, o.created_date, o.complete, o.past, h.id, h.name, ho.cancelled
        order by o.id desc, h.name asc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, hoi.household_id, p.id, p.name, hoi.quantity, p.price * hoi.quantity as total
        from household_order_item hoi
        inner join product p on p.id = hoi.product_id
        order by p.name asc
      |]
      return (os :: [(Int, Day, Bool, Bool, Int, String, Bool, Int)], is :: [(Int, Int, Int, String, Int, Int)])
    close conn
    return $ rOrders <&> \(orderId, orderCreated, orderComplete, orderPast, householdId, name, cancelled, total) ->
      let item (_, _, productId, name, quantity, total) = HouseholdOrderItem productId name quantity total
          thisOrder (oId, hId, _, _, _, _) = oId == orderId && hId == householdId
          items = map item $ filter thisOrder rItems
      in  HouseholdOrder orderId orderCreated orderComplete orderPast householdId name cancelled total items

  getProducts :: ByteString -> IO [Product]
  getProducts connectionString = do
    conn <- connectPostgreSQL connectionString
    rProducts <- query_ conn [sql|
      select p.id, p.name, p.price
      from product p
      where p.archived = false
      order by p.name asc
    |]
    close conn
    return $ (rProducts :: [(Int, String, Int)]) <&> \(id, name, price) -> Product id name price

  getHouseholds :: ByteString -> IO [Household]
  getHouseholds connectionString = do
    conn <- connectPostgreSQL connectionString
    rHouseholds <- query_ conn [sql|
      select h.id, h.name
      from household h
      where h.archived = false
      order by h.name asc
    |]
    close conn
    return $ (rHouseholds :: [(Int, String)]) <&> \(id, name) -> Household id name

  getHouseholdPayments :: ByteString -> IO [HouseholdPayment]
  getHouseholdPayments connectionString = do
    conn <- connectPostgreSQL connectionString
    rPayments <- query_ conn [sql|
      select p.id, p.household_id, p.date, p.amount
      from household_payment p
      where p.archived = false
      order by p.id asc
    |]
    close conn
    return $ (rPayments :: [(Int, Int, Day, Int)]) <&> \(id, householdId, date, amount) -> HouseholdPayment id householdId date amount

  createOrder :: ByteString -> Day -> Maybe Int -> IO Int
  createOrder connectionString date maybeHouseholdId = do
    conn <- connectPostgreSQL connectionString
    id <- withTransaction conn $ do
      [Only id] <- query conn [sql|
        insert into "order" (created_date, complete, cancelled) values (?, false, false) returning id
      |] (Only date)
      case maybeHouseholdId of
        Just householdId -> void $ execute conn [sql|
          insert into household_order (order_id, household_id, cancelled) values (?, ?, false)
        |] (id, householdId)
        _ -> return ()
      return id
    close conn
    return id

  ensureHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> Int -> IO ()
  ensureHouseholdOrderItem connectionString orderId householdId productId quantity = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      exists <- query conn [sql|
        select 1 from household_order_item where order_id = ? and household_id = ? and product_id = ?
      |] (orderId, householdId, productId)
      if null (exists :: [Only Int]) then
        execute conn [sql|
          insert into household_order_item (order_id, household_id, product_id, quantity) values (?, ?, ?, ?)
        |] (orderId, householdId, productId, quantity)
      else
        execute conn [sql|
          update household_order_item set quantity = ? where order_id = ? and household_id = ? and product_id = ?
        |] (quantity, orderId, householdId, productId)  
    close conn

  removeHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> IO ()
  removeHouseholdOrderItem connectionString orderId householdId productId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order_item where order_id = ? and household_id = ? and product_id = ?
    |] (orderId, householdId, productId)
    close conn

  addHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  addHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      insert into household_order (order_id, household_id, cancelled) values (?, ?, false)
    |] (orderId, householdId)
    close conn

  cancelHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  cancelHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set cancelled = true where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  uncancelHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  uncancelHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set cancelled = false where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn
  
  createHousehold :: ByteString -> String -> IO Int
  createHousehold connectionString name = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household (name, archived) values (?, false) returning id
    |] (Only name)
    close conn
    return id
  
  archiveHousehold :: ByteString -> Int -> IO ()
  archiveHousehold connectionString householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set archived = true where id = ?
    |] (Only householdId)
    close conn

  createProduct :: ByteString -> String -> Int -> IO Int
  createProduct connectionString name price = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into product (name, price, archived) values (?, ?, false) returning id
    |] (name, price)
    close conn
    return id
  
  archiveProduct :: ByteString -> Int -> IO ()
  archiveProduct connectionString productId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update product set archived = true where id = ?
    |] (Only productId)
    close conn