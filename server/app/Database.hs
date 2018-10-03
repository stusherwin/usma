{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Database ( getCollectiveOrders, getHouseholdOrders, getProducts, getHouseholds, getHouseholdPayments, getProductCatalogue
                , createOrder, deleteOrder, placeOrder, unplaceOrder
                , createHouseholdOrder, deleteHouseholdOrder, cancelHouseholdOrder, completeHouseholdOrder, reopenHouseholdOrder
                , ensureHouseholdOrderItem, removeHouseholdOrderItem
                , createHousehold, updateHousehold, archiveHousehold
                , createHouseholdPayment, updateHouseholdPayment, archiveHouseholdPayment
                , replaceProductCatalogue
                ) where
  import Control.Monad (mzero, when, void)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.ToRow
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Database.PostgreSQL.Simple.SqlQQ
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.Time.Calendar (Day, showGregorian)
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment
  import ProductCatalogueData
  import ProductCatalogueEntry
  
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

  instance FromRow ProductCatalogueData where
    fromRow = ProductCatalogueData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

  instance ToRow ProductCatalogueData where
    toRow e = [toField $ ProductCatalogueData.code e, toField $ category e, toField $ brand e, toField $ description e, toField $ text e, toField $ size e, toField $ ProductCatalogueData.price e, toField $ ProductCatalogueData.vatRate e, toField $ rrp e, toField $ biodynamic e, toField $ fairTrade e, toField $ glutenFree e, toField $ organic e, toField $ addedSugar e, toField $ vegan e, toField $ priceChanged e]

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
               select o.id, o.created_date, o.placed, o.past, coalesce(bool_and(ho.complete), false) as complete, coalesce(bool_and(ho.cancelled), false) as cancelled
               from "order" o
               left join household_order ho on ho.order_id = o.id
               group by o.id, o.created_date
               order by o.id desc
             )
        select o.id, o.created_date, o.complete, o.cancelled, o.placed, o.past, coalesce(sum(p.price * hoi.quantity), 0) as total
        from orders o
        left join household_order ho on ho.order_id = o.id and ho.cancelled = false
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        group by o.id, o.created_date, o.complete, o.cancelled, o.placed, o.past
        order by o.id desc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, p.id, p.code, p.name, p.price, p.vat_rate, sum(hoi.quantity) as quantity, sum(p.price * hoi.quantity) as total
        from household_order_item hoi
        inner join product p on p.id = hoi.product_id
        group by hoi.order_id, p.id, p.code, p.name, p.price, p.vat_rate
        order by p.code asc
      |]
      return (os :: [(Int, Day, Bool, Bool, Bool, Bool, Int)], is :: [(Int, Int, String, String, Int, VatRate, Int, Int)])
    close conn
    return $ rOrders <&> \(id, created, complete, cancelled, placed, past, total) ->
      let item (_, productId, code, name, price, vatRate, quantity, total) = CollectiveOrderItem productId code name price vatRate quantity total
          thisOrder (oId, _, _, _, _, _, _, _) = oId == id
          items = map item $ filter thisOrder rItems
      in  collectiveOrder id created complete cancelled placed past total items
  
  getHouseholdOrders :: ByteString -> IO [HouseholdOrder]
  getHouseholdOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    (rOrders, rItems) <- withTransaction conn $ do
      os <- query_ conn [sql|
        with orders as (
               select o.id, o.created_date, o.placed, o.past
               from "order" o
               order by o.id desc
             )
        select o.id, o.created_date, o.placed, o.past, h.id, h.name, ho.complete, ho.cancelled, coalesce(sum(p.price * hoi.quantity), 0) as total
        from household_order ho
        inner join orders o on o.id = ho.order_id
        inner join household h on h.id = ho.household_id
        left join household_order_item hoi on hoi.order_id = ho.order_id and hoi.household_id = ho.household_id
        left join product p on p.id = hoi.product_id
        group by o.id, o.created_date, o.placed, o.past, h.id, h.name, ho.complete, ho.cancelled
        order by o.id desc, h.name asc
      |]
      is <- query_ conn [sql|
        select hoi.order_id, hoi.household_id, p.id, p.code, p.name, p.price, p.vat_rate, hoi.quantity, p.price * hoi.quantity as total
        from household_order_item hoi
        inner join product p on p.id = hoi.product_id
        order by p.code asc
      |]
      return (os :: [(Int, Day, Bool, Bool, Int, String, Bool, Bool, Int)], is :: [(Int, Int, Int, String, String, Int, VatRate, Int, Int)])
    close conn
    return $ rOrders <&> \(orderId, orderCreated, orderPlaced, orderPast, householdId, householdName, complete, cancelled, total) ->
      let item (_, _, productId, code, name, price, vatRate, quantity, total) = HouseholdOrderItem productId code name price vatRate quantity total
          thisOrder (oId, hId, _, _, _, _, _, _, _) = oId == orderId && hId == householdId
          items = map item $ filter thisOrder rItems
      in  householdOrder orderId orderCreated orderPlaced orderPast householdId householdName complete cancelled total items

  getProducts :: ByteString -> IO [Product]
  getProducts connectionString = do
    conn <- connectPostgreSQL connectionString
    rProducts <- query_ conn [sql|
      select p.id, p.code, p.name, p.price, p.vat_rate, p.price_updated
      from product p
      where p.archived = false
      order by p.code asc
    |]
    close conn
    return $ (rProducts :: [(Int, String, String, Int, VatRate, Maybe Day)]) <&> \(id, code, name, price, vatRate, priceUpdated) -> Product id code name price vatRate priceUpdated

  getHouseholds :: ByteString -> IO [Household]
  getHouseholds connectionString = do
    conn <- connectPostgreSQL connectionString
    rHouseholds <- query_ conn [sql|
        with household_total_orders as (
               select h.id, coalesce(sum(p.price * hoi.quantity), 0) as total
               from household h
               left join household_order ho on ho.household_id = h.id and ho.cancelled = false
               left join household_order_item hoi on hoi.household_id = ho.household_id and hoi.order_id = ho.order_id
               left join product p on p.id = hoi.product_id
               group by h.id
             ),
             household_total_payments as (
               select h.id, coalesce(sum(hp.amount), 0) as total
               from household h
               left join household_payment hp on hp.household_id = h.id
               group by h.id
             )
      select h.id, h.name, hto.total as total_orders, htp.total as total_payments, htp.total - hto.total as balance
      from household h
      inner join household_total_orders hto on hto.id = h.id
      inner join household_total_payments htp on htp.id = h.id
      where h.archived = false
      order by h.name asc
    |]
    close conn
    return $ (rHouseholds :: [(Int, String, Int, Int, Int)]) <&> \(id, name, totalOrders, totalPayments, balance) -> Household id name totalOrders totalPayments balance

  getHouseholdPayments :: ByteString -> IO [HouseholdPayment]
  getHouseholdPayments connectionString = do
    conn <- connectPostgreSQL connectionString
    rPayments <- query_ conn [sql|
      select p.id, p.household_id, p."date", p.amount
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
      execute_ conn [sql|
        update "order" set past = true
      |]
      [Only id] <- query conn [sql|
        insert into "order" (created_date, placed, past) values (?, false, false) returning id
      |] (Only date)
      case maybeHouseholdId of
        Just householdId -> void $ execute conn [sql|
          insert into household_order (order_id, household_id, complete, cancelled) values (?, ?, false, false)
        |] (id, householdId)
        _ -> return ()
      return id
    close conn
    return id

  deleteOrder :: ByteString -> Int -> IO ()
  deleteOrder connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from "order" where id = ?
    |] (Only orderId)
    close conn

  placeOrder :: ByteString -> Int -> IO ()
  placeOrder connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update "order" set placed = true where id = ?
    |] (Only orderId)
    close conn

  unplaceOrder :: ByteString -> Int -> IO ()
  unplaceOrder connectionString orderId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update "order" set placed = false where id = ?
    |] (Only orderId)
    close conn

  createHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  createHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      insert into household_order (order_id, household_id, complete, cancelled) values (?, ?, false, false)
    |] (orderId, householdId)
    close conn

  deleteHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  deleteHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      delete from household_order where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  cancelHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  cancelHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute conn [sql|
        update household_order set cancelled = true where order_id = ? and household_id = ?
      |] (orderId, householdId)
      exists <- query conn [sql|
        select 1 from household_order where order_id = ? and cancelled = false
      |] (Only orderId)
      if null (exists :: [Only Int]) then
        void $ execute conn [sql|
          update "order" set past = true where id = ?
        |] (Only orderId)
      else return ()
    close conn

  completeHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  completeHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set complete = true where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  reopenHouseholdOrder :: ByteString -> Int -> Int -> IO ()
  reopenHouseholdOrder connectionString orderId householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_order set cancelled = false, complete = false where order_id = ? and household_id = ?
    |] (orderId, householdId)
    close conn

  ensureHouseholdOrderItem :: ByteString -> Int -> Int -> String -> HouseholdOrderItemDetails -> IO ()
  ensureHouseholdOrderItem connectionString orderId householdId productCode details = do
    let quantity = hoidQuantity details
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      productId <- do
        ids <- query conn [sql|
          select id from product where code = ?
        |] (Only productCode)
        case ids of
          (Only id):xs -> return id
          _ -> do
            [Only id] <- query conn [sql|
              insert into product ("code", "name", price, vat_rate, archived) 
              select ce.code
                   , concat_ws(' ', nullif(btrim(ce.brand), '')
                                  , nullif(btrim(ce."description"), '')
                                  , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                                  , nullif(btrim(ce."text"), ''))
                   , ce.price
                   , ce.vat_rate
                   , false
              from catalogue_entry ce
              where ce.code = ?
              returning id
            |] (Only productCode)
            return (id :: Int)
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

  createHousehold :: ByteString -> HouseholdDetails -> IO Int
  createHousehold connectionString details = do
    let name = hdName details
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household (name, archived) values (?, false) returning id
    |] (Only name)
    close conn
    return id
  
  updateHousehold :: ByteString -> Int -> HouseholdDetails -> IO ()
  updateHousehold connectionString householdId details = do
    let name = hdName details
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set name = ? where id = ?
    |] (name, householdId)
    close conn

  archiveHousehold :: ByteString -> Int -> IO ()
  archiveHousehold connectionString householdId = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household set archived = true where id = ?
    |] (Only householdId)
    close conn

  createHouseholdPayment :: ByteString -> Int -> HouseholdPaymentDetails -> IO Int
  createHouseholdPayment connectionString householdId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn [sql|
      insert into household_payment (household_id, "date", amount, archived) values (?, ?, ?, false) returning id
    |] (householdId, date, amount)
    close conn
    return id

  updateHouseholdPayment :: ByteString -> Int -> HouseholdPaymentDetails -> IO ()
  updateHouseholdPayment connectionString paymentId details = do
    let date = hpdDate details
    let amount = hpdAmount details
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_payment set "date" = ?, amount = ? where id = ?
    |] (date, amount, paymentId)
    close conn
  
  archiveHouseholdPayment :: ByteString -> Int -> IO ()
  archiveHouseholdPayment connectionString id = do
    conn <- connectPostgreSQL connectionString
    execute conn [sql|
      update household_payment set archived = true where id = ?
    |] (Only id)
    close conn

  getProductCatalogue :: ByteString -> IO [ProductCatalogueEntry]
  getProductCatalogue connectionString = do
    conn <- connectPostgreSQL connectionString
    rEntries <- query_ conn [sql|
      select ce.code
           , concat_ws(' ', nullif(btrim(ce.brand), '')
                          , nullif(btrim(ce."description"), '')
                          , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                          , nullif(btrim(ce."text"), ''))
           , ce.price
           , ce.vat_rate
      from catalogue_entry ce
    |]
    close conn
    return $ (rEntries :: [(String, String, Int, VatRate)]) <&> \(code, name, price, vatRate) -> ProductCatalogueEntry code name price vatRate

  replaceProductCatalogue :: ByteString -> Day -> [ProductCatalogueData] -> IO ()
  replaceProductCatalogue connectionString date entries = do
    conn <- connectPostgreSQL connectionString
    withTransaction conn $ do
      execute_ conn [sql|
        truncate table catalogue_entry
      |]
      void $ executeMany conn [sql|
        insert into catalogue_entry (code, category, brand, "description", "text", size, price, vat_rate, rrp, biodynamic, fair_trade, gluten_free, organic, added_sugar, vegan, price_changed)
        values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |] entries
      execute conn [sql|
        update product p
        set "name" = concat_ws(' ', nullif(btrim(ce.brand), '')
                                  , nullif(btrim(ce."description"), '')
                                  , nullif('(' || lower(btrim(ce.size)) || ')', '()')
                                  , nullif(btrim(ce."text"), ''))
          , price = ce.price
          , vat_rate = ce.vat_rate
          , price_updated = case when ce.price <> p.price then ? else p.price_updated end
        from catalogue_entry ce
        where ce.code = p.code
      |] (Only date)
    close conn