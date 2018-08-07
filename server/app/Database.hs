{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Database (getAllOrders, getOrderSummary, getHouseholdOrderSummary, createOrder, deleteOrder, ensureHouseholdOrderItem, removeHouseholdOrderItem) where
  import Control.Monad (mzero, when)
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
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  infixr 0 &
  
  getAllOrders :: ByteString -> IO [Order]
  getAllOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query_ conn [sql|
      select o.id, o.created_date, o.complete, coalesce(sum(p.price * hoi.quantity), 0) as total
      from "order" o
      left join household_order_item hoi on hoi.order_id = o.id
      left join product p on p.id = hoi.product_id
      group by o.id, o.created_date, o.complete
    |]
    close conn
    return $ rOrders <&> order
    where
    order :: (Int, Day, Bool, Int) -> Order
    order (id, createdDate, complete, total) = Order id (showGregorian createdDate) complete total

  getOrderSummary :: ByteString -> Int -> IO (Maybe OrderSummary)
  getOrderSummary connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query conn [sql|
      select o.id, o.created_date, o.complete, coalesce(sum(p.price * hoi.quantity), 0) as total
      from "order" o
      left join household_order_item hoi on hoi.order_id = o.id
      left join product p on p.id = hoi.product_id
      where o.id = ?
      group by o.id, o.created_date, o.complete
    |] (Only orderId)
    rHouseholds <- query conn [sql|
      select h.id, h.name, ho.order_id, ho.status, coalesce(sum(p.price * hoi.quantity), 0) as total
      from household_order ho
      inner join household h on h.id = ho.household_id
      left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
      left join product p on p.id = hoi.product_id
      where ho.order_id = ?
      group by h.id, ho.order_id, h.name, ho.status
    |] (Only orderId)
    close conn
    return $ listToMaybe $ orderSummary rHouseholds <$> rOrders
    where    
    orderSummary :: [ROrderSummary_Household] -> (Int, Day, Bool, Int) -> OrderSummary
    orderSummary rHouseholds (id, createdDate, complete, total) = 
      let households = filter ((==) id . rHouseholdOrderId) rHouseholds <&> household
      in OrderSummary (showGregorian createdDate) complete total households

    household :: ROrderSummary_Household -> OrderSummary_Household
    household (id, name, _, status, total) = OrderSummary_Household id name (householdOrderStatus status) total
  
  type ROrderSummary_Household = (Int, String, Int, Char, Int)
  rHouseholdOrderId (_, _, x, _, _) = x

  getHouseholdOrderSummary :: ByteString -> Int -> Int -> IO (Maybe HouseholdOrderSummary)
  getHouseholdOrderSummary connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    rHouseholds <- query conn [sql|
      select h.id, h.name, ho.status, o.created_date, coalesce(sum(p.price * hoi.quantity), 0)
      from household_order ho
      inner join "order" o on o.id = ho.order_id
      inner join household h on h.id = ho.household_id
      left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
      left join product p on p.id = hoi.product_id
      where ho.order_id = ? and ho.household_id = ?
      group by h.id, h.name, ho.status, o.created_date
    |] (orderId, householdId)
    rItems <- query conn [sql|
      select p.id, p.name, hoi.household_id, hoi.quantity, p.price * hoi.quantity as total
      from household_order_item hoi
      inner join product p on p.id = hoi.product_id
      where hoi.order_id = ? and hoi.household_id = ?
    |] (orderId, householdId)
    close conn
    return $ listToMaybe $ householdOrderSummary rItems <$> rHouseholds
    where
    householdOrderSummary :: [RHouseholdOrderSummary_Item] -> (Int, String, Char, Day, Int) -> HouseholdOrderSummary
    householdOrderSummary rItems (id, name, status, date, total) =
      let items = filter ((==) id . rItemHouseholdId) rItems <&> item
      in HouseholdOrderSummary (showGregorian date) name (householdOrderStatus status) total items

    item :: RHouseholdOrderSummary_Item -> HouseholdOrderSummary_Item
    item (id, name, _, quantity, total) = HouseholdOrderSummary_Item id name quantity total

  type RHouseholdOrderSummary_Item = (Int, String, Int, Int, Int)
  rItemHouseholdId (_, _, x, _, _) = x

  createOrder :: ByteString -> Day -> IO Int
  createOrder connectionString date = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into "order" (created_date, complete) values (?, false) returning id
    |] (Only date)
    close conn
    return id

  deleteOrder :: ByteString -> Int -> IO ()
  deleteOrder connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql| 
        delete from houseold_order_item hoi where order_id = ?
      |] (Only orderId)
      execute conn [sql| 
        delete from houseold_order ho where order_id = ?
      |] (Only orderId)
      execute conn [sql|
        delete from "order" where id = ?
      |] (Only orderId)
    close conn
    return ()

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
    return ()

  removeHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> IO ()
  removeHouseholdOrderItem connectionString orderId householdId productId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order_item where order_id = ? and household_id = ? and product_id = ?
    |] (orderId, householdId, productId)
    close conn
    return ()

  updateHouseholdOrderItem :: ByteString -> Int -> Int -> Int -> Int -> IO ()
  updateHouseholdOrderItem connectionString orderId householdId productId quantity = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order_item set quantity = ? where order_id = ? and household_id = ? and product_id = ?
    |] (quantity, orderId, householdId, productId)
    close conn
    return ()

  householdOrderStatus :: Char -> HouseholdOrderStatus
  householdOrderStatus 'P' = Paid
  householdOrderStatus 'C' = Cancelled
  householdOrderStatus _   = Unpaid