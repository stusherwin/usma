{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module V2.Repository.SQL where 

import           Control.Monad (mzero, void)
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
import           Data.Functor ((<&>))
import           Data.List (foldl')
import qualified Data.Text as T (pack)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Database.PostgreSQL.Simple (Connection, Only(..), Query, Binary(..), (:.)(..), query, query_, execute, executeMany)
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow(..), RowParser, field)
import           Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import V2.Domain

selectOrderGroupId :: Connection -> String -> IO [Only OrderGroupId]
selectOrderGroupId conn groupKey = do
  query conn [sql|
    select id
    from v2.order_group
    where key = ?
  |] (Only groupKey)

selectOrderGroup :: Connection -> OrderGroupId -> IO [OrderGroup]
selectOrderGroup conn groupId = 
  query conn [sql|
    select id
         , name
         , key
         , is_payments_enabled
    from v2.order_group
    where id = ?
  |] (Only groupId)

selectVatRates :: Connection -> IO [VatRate]
selectVatRates conn = 
  query_ conn ([sql|
    select code, multiplier
    from v2.vat_rate
  |])

selectProductCatalogueFile :: Connection -> IO [(UTCTime, Text)]
selectProductCatalogueFile conn =
  query_ conn [sql|
    select uploaded_date, "file"
    from v2.catalogue_file
    order by uploaded_date desc
    limit 1
  |]

insertProductCatalogueFile :: Connection -> UTCTime -> Text -> IO ()
insertProductCatalogueFile conn date file = 
  void $ execute conn [sql|
    insert into v2.catalogue_file ( uploaded_date, "file" ) 
    values (?, ?)
  |] (date, file)

selectProductData :: Connection -> ProductCode -> IO [(Maybe ByteString, Maybe String, Maybe String, Maybe String, Maybe Int)]
selectProductData conn code =
  query conn [sql|
    select image, url, title, imageUrl, size
    from v2.product_data
    where code = ?
  |] (Only code)

insertProductData :: Connection -> ProductCode -> Maybe ByteString -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> IO ()
insertProductData conn code image url title imageUrl size = 
  void $ execute conn [sql|
    insert into v2.product_data (code, image, url, title, imageUrl, size)
    values (?, ?, ?, ?, ?, ?)
    ON CONFLICT (code) DO UPDATE SET
      image = EXCLUDED.image,
      url = EXCLUDED.url,
      title = EXCLUDED.title,
      imageUrl = EXCLUDED.imageUrl,
      size = EXCLUDED.size;
  |] (code, Binary <$> image, url, title, imageUrl, size)

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
selectProducts :: Connection -> [WhereParam] -> IO [(ProductCode, ProductId)]
selectProducts conn whereParams = 
    query conn ([sql|
      select distinct p.code
                    , p.id
      from v2.product p 
      join v2.order_item oi on oi.product_code = p.code
      join v2."order" o on oi.order_id = o.id
      where 1 = 1 |] <> whereClause <> [sql|
      order by p.code
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForProductCode _)     -> Just [sql| p.code = ? |]
      (ForProductId _)   -> Just [sql| p.id = ? |]
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      (ForOrder _)       -> Just [sql| oi.id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      _ -> Nothing

-- Needed to convert ProductId to ProductCode
-- TODO: Remove ProductId altogether
insertProducts :: Connection -> [ProductCode] -> IO ()
insertProducts conn codes =
  void $ executeMany conn [sql|
    insert into v2.product 
      ( "code"
      )
    values (?)
  |] $ map Only codes

selectHouseholdRows :: Connection -> [WhereParam] -> IO [HouseholdRow]
selectHouseholdRows conn whereParams = 
    query conn ([sql|
      select h.id
           , h.name
           , h.contact_name
           , h.contact_email
           , h.contact_phone
      from v2.household h
      where h.is_archived = false |] <> whereClause <> [sql| 
      order by h.name asc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| h.order_group_id = ? |]
      (ForHousehold _)  -> Just [sql| h.id = ? |]
      _ -> Nothing

insertHousehold :: Connection -> OrderGroupId -> HouseholdSpec -> IO [Only HouseholdId]
insertHousehold conn groupId spec = 
  query conn [sql|
    insert into v2.household 
      ( order_group_id
      , name
      , contact_name
      , contact_email
      , contact_phone
      , is_archived
      ) 
    values (?, ?, ?, ?, ?, false)
    returning id
  |] ( groupId
     , _householdSpecName spec
     , _contactName . _householdSpecContact $ spec
     , _contactEmail . _householdSpecContact $ spec
     , _contactPhone . _householdSpecContact $ spec
     )

updateHouseholds :: Connection -> OrderGroupId -> [Household] -> IO ()
updateHouseholds conn groupId households = do
  let rows = households <&> \h -> ( _householdName . _householdInfo $ h
                                  , _contactName . _householdContact $ h
                                  , _contactEmail . _householdContact $ h
                                  , _contactPhone . _householdContact $ h
                                  , _householdId . _householdInfo $ h
                                  , groupId
                                  )
  void $ executeMany conn [sql|
    update v2.household h
    set name = u.name
      , contact_name = u.contact_name
      , contact_email = u.contact_email
      , contact_phone = u.contact_phone
    from (values (?, ?, ?, ?, ?, ?)) as u
    ( name
    , contact_name
    , contact_email
    , contact_phone
    , id
    , order_group_id
    )
    where h.id = u.id
      and h.order_group_id = u.order_group_id
  |] rows

deleteHousehold :: Connection -> OrderGroupId -> HouseholdId -> IO ()
deleteHousehold conn groupId householdId = 
  void $ execute conn [sql|
    update v2.household
    set is_archived = true
    where id = ?
      and order_group_id = ?
  |] ( householdId
     , groupId
     )

selectPayments :: Connection -> [WhereParam] -> IO [Payment]
selectPayments conn whereParams = 
    query conn ([sql|
      select p.id
           , p.household_id
           , p."date"
           , p.amount
      from v2.payment p
      where p.is_archived = false |] <> whereClause <> [sql|
      order by p.id asc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| p.order_group_id = ? |]
      (ForHousehold _)  -> Just [sql| p.household_id = ? |]
      (ForPayment _)  -> Just [sql| p.id = ? |]
      _ -> Nothing

insertPayment :: Connection -> OrderGroupId -> PaymentSpec -> IO [Only PaymentId]
insertPayment conn groupId spec = 
  query conn [sql|
    insert into v2.payment 
      ( order_group_id
      , household_id
      , "date"
      , amount
      , is_archived
      ) 
    values (?, ?, ?, ?, false)
    returning id
  |] ( groupId
     , _paymentSpecHouseholdId spec
     , _paymentSpecDate spec
     , _paymentSpecAmount spec
     )

updatePayments :: Connection -> OrderGroupId -> [Payment] -> IO ()
updatePayments conn groupId payments = do
  let rows = payments <&> \p -> ( _paymentDate p
                                , _paymentAmount p
                                , _paymentId p
                                , groupId
                                )
  void $ executeMany conn [sql|
    update v2.payment p
    set "date" = cast(u."date" as timestamptz)
      , amount = u.amount
    from (values (?, ?, ?, ?)) as u
    ( "date"
    , amount
    , id
    , order_group_id
    )
    where p.id = u.id
      and p.order_group_id = u.order_group_id
  |] rows

deletePayment :: Connection -> OrderGroupId -> PaymentId -> IO ()
deletePayment conn groupId paymentId = 
  void $ execute conn [sql|
    update v2.payment 
    set is_archived = true
    where id = ?
      and order_group_id = ?
  |] ( paymentId
     , groupId
     )

selectOrderRows :: Connection -> [WhereParam] -> IO [OrderRow]
selectOrderRows conn whereParams = do
    query conn ([sql|
      select o.id
           , o.order_group_id
           , o.created
           , cb.id as created_by_household_id
           , cb.name as created_by_household_name
           , o.is_abandoned
           , o.is_placed
      from v2."order" o
      left join v2.household cb
        on cb.id = o.created_by_id 
      where 1 = 1 |] <> whereClause <> [sql|
      order by o.id desc
    |]) params
  where 
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      _ -> Nothing

insertOrder :: Connection -> OrderGroupId -> OrderSpec -> IO [Only OrderId]
insertOrder conn groupId spec = 
  query conn [sql|
    insert into v2."order" (order_group_id, created, created_by_id, is_abandoned, is_placed) 
    values (?, ?, ?, false, false) 
    returning id
  |] (groupId, _orderSpecCreated spec, _orderSpecCreatedByHouseholdId spec)

updateOrders :: Connection -> [Order] -> IO ()
updateOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _orderInfo $ o
                                  orderId = _orderId . _orderInfo $ o
                                  abandoned = (== OrderAbandoned) . _orderStatus $ o
                                  placed = (== OrderPlaced) . _orderStatus $ o
                                  --complete?
                              in (abandoned, placed, {- complete?, -} groupId, orderId)
  void $ executeMany conn [sql|
    update v2."order" o
    set is_abandoned = u.is_abandoned
      , is_placed = u.is_placed
    from (values (?, ?, ?, ?)) as u
    ( is_abandoned
    , is_placed
    , order_group_id
    , id
    )
    where o.id = u.id
      and o.order_group_id = u.order_group_id
  |] rows

selectHouseholdOrderRows :: Connection -> [WhereParam] -> IO [HouseholdOrderRow]
selectHouseholdOrderRows conn whereParams = 
    query conn ([sql|
      select o.id as order_id 
           , o.order_group_id
           , o.created
           , cb.id as created_by_household_id
           , cb.name as created_by_household_name
           , o.is_abandoned
           , o.is_placed
           , h.id as household_id
           , h.name as household_name
           , ho.is_abandoned
           , ho.is_complete
      from v2.household_order ho
      inner join v2."order" o 
        on o.id = ho.order_id
      left join v2.household cb
        on cb.id = o.created_by_id 
      inner join v2.household h 
        on h.id = ho.household_id
      where 1 = 1 |] <> whereClause <> [sql|
      order by o.id desc, h.name asc
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      (ForHousehold _)   -> Just [sql| h.id = ? |]
      _ -> Nothing

insertHouseholdOrders :: Connection -> [HouseholdOrder] -> IO ()
insertHouseholdOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                                  orderId = _orderId . _householdOrderOrderInfo $ o
                                  householdId = _householdId . _householdOrderHouseholdInfo $ o
                                  abandoned = (== HouseholdOrderAbandoned) . _householdOrderStatus $ o
                                  complete = (== HouseholdOrderComplete) . _householdOrderStatus $ o
                              in (abandoned, complete, groupId, orderId, householdId)
  void $ executeMany conn [sql|
    insert into v2.household_order (is_abandoned, is_complete, order_group_id, order_id, household_id) 
    values (?, ?, ?, ?, ?)
  |] rows

updateHouseholdOrders :: Connection -> [HouseholdOrder] -> IO ()
updateHouseholdOrders conn orders = do
  let rows = orders <&> \o -> let groupId = _orderGroupId . _householdOrderOrderInfo $ o
                                  orderId = _orderId . _householdOrderOrderInfo $ o
                                  householdId = _householdId . _householdOrderHouseholdInfo $ o
                                  abandoned = (== HouseholdOrderAbandoned) . _householdOrderStatus $ o
                                  complete = (== HouseholdOrderComplete) . _householdOrderStatus $ o
                              in (abandoned, complete, groupId, orderId, householdId)
  void $ executeMany conn [sql|
    update v2.household_order ho
    set is_abandoned = u.is_abandoned
      , is_complete = u.is_complete
    from (values (?, ?, ?, ?, ?)) as u
    ( is_abandoned
    , is_complete
    , order_group_id
    , order_id
    , household_id
    )
    where ho.order_group_id = u.order_group_id 
      and ho.order_id = u.order_id 
      and ho.household_id = u.household_id
  |] rows

selectOrderItemRows :: Connection -> [WhereParam] -> IO [(OrderId, HouseholdId) :. OrderItemRow]
selectOrderItemRows conn whereParams = 
    query conn ([sql|
      select hoi.order_id
           , hoi.household_id
           , hoi.product_code
           , hoi.product_name
           , hoi.product_vat_rate
           , hoi.product_vat_rate_multiplier
           , hoi.product_price
           , hoi.product_is_biodynamic
           , hoi.product_is_fair_trade
           , hoi.product_is_gluten_free
           , hoi.product_is_organic
           , hoi.product_is_added_sugar
           , hoi.product_is_vegan
           , hoi.quantity
           , adj.new_vat_rate 
           , adjv.multiplier
           , adj.new_price
           , adj.new_quantity
           , adj.is_discontinued
           , adj.date
           , hoi.is_packed
      from v2.order_item hoi
      inner join v2.household_order ho 
        on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
      inner join v2."order" o
        on ho.order_id = o.id
      left join v2.product p
        on hoi.product_code = p.code
      left join v2.order_item_adjustment adj
        on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_code = adj.product_code
      left join v2.vat_rate adjv
        on adjv.code = adj.new_vat_rate
      where 1 = 1 |] <> whereClause <> [sql|
      order by hoi.product_code
    |]) params
  where
    (whereClause, params) = toWhereClause whereParams $ \case
      (ForOrderGroup _)  -> Just [sql| o.order_group_id = ? |]
      OrderIsCurrent     -> Just [sql| o.is_abandoned = 'f' and o.is_placed = 'f' |]
      OrderIsPast        -> Just [sql| o.is_abandoned = 't' or o.is_placed = 't' |]
      (ForOrder _)       -> Just [sql| o.id = ? |]
      (ForHousehold _)   -> Just [sql| ho.household_id = ? |]
      _ -> Nothing

insertOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItem)] -> IO ()
insertOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, productCode), i) -> 
                         UpdateOrderItem (_itemQuantity i)
                                         (_productName . _productInfo . _itemProduct $ i)
                                         (_vatRateType . _priceVatRate . itemProductPrice $ i)
                                         ((realToFrac $ _vatRateMultiplier . _priceVatRate . itemProductPrice $ i) :: Double)
                                         (_moneyExcVat . _priceAmount . itemProductPrice $ i)
                                         (_productIsBiodynamic . _productFlags . _itemProduct $ i)
                                         (_productIsFairTrade  . _productFlags . _itemProduct $ i)
                                         (_productIsGlutenFree . _productFlags . _itemProduct $ i)
                                         (_productIsOrganic    . _productFlags . _itemProduct $ i)
                                         (_productIsAddedSugar . _productFlags . _itemProduct $ i)
                                         (_productIsVegan      . _productFlags . _itemProduct $ i)
                                         groupId
                                         orderId
                                         householdId
                                         productCode
                                         (_itemIsPacked i)
  void $ executeMany conn [sql|
    insert into v2.order_item 
      ( quantity
      , product_name
      , product_vat_rate
      , product_vat_rate_multiplier
      , product_price
      , product_is_biodynamic
      , product_is_fair_trade
      , product_is_gluten_free
      , product_is_organic
      , product_is_added_sugar
      , product_is_vegan
      , order_group_id
      , order_id
      , household_id
      , product_code
      , is_packed
      )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] rows

updateOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItem)] -> IO ()
updateOrderItems conn items = do
  let rows = items <&> \((groupId, orderId, householdId, productCode), i) -> 
                         UpdateOrderItem (_itemQuantity i)
                                         (_productName . _productInfo . _itemProduct $ i)
                                         (_vatRateType . _priceVatRate . itemProductPrice $ i)
                                         ((realToFrac $ _vatRateMultiplier . _priceVatRate . itemProductPrice $ i) :: Double)
                                         (_moneyExcVat . _priceAmount . itemProductPrice $ i)
                                         (_productIsBiodynamic . _productFlags . _itemProduct $ i)
                                         (_productIsFairTrade  . _productFlags . _itemProduct $ i)
                                         (_productIsGlutenFree . _productFlags . _itemProduct $ i)
                                         (_productIsOrganic    . _productFlags . _itemProduct $ i)
                                         (_productIsAddedSugar . _productFlags . _itemProduct $ i)
                                         (_productIsVegan      . _productFlags . _itemProduct $ i)
                                         groupId
                                         orderId
                                         householdId
                                         productCode
                                         (_itemIsPacked i)
  void $ executeMany conn [sql|
    update v2.order_item i
    set quantity = u.quantity
      , product_name = u.product_name
      , product_vat_rate = u.product_vat_rate
      , product_vat_rate_multiplier = u.product_vat_rate_multiplier
      , product_price = u.product_price
      , product_is_biodynamic = u.product_is_biodynamic
      , product_is_fair_trade = u.product_is_fair_trade
      , product_is_gluten_free = u.product_is_gluten_free
      , product_is_organic = u.product_is_organic
      , product_is_added_sugar = u.product_is_added_sugar
      , product_is_vegan = u.product_is_vegan
      , is_packed = u.is_packed
    from (values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)) as u
    ( quantity
    , product_name
    , product_vat_rate
    , product_vat_rate_multiplier
    , product_price
    , product_is_biodynamic
    , product_is_fair_trade
    , product_is_gluten_free
    , product_is_organic
    , product_is_added_sugar
    , product_is_vegan
    , order_group_id
    , order_id
    , household_id
    , product_code
    , is_packed
    )
    where i.order_group_id = u.order_group_id
      and i.order_id = u.order_id
      and i.household_id = u.household_id
      and i.product_code = u.product_code
  |] rows

deleteOrderItems :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItem)] -> IO ()
deleteOrderItems conn items = do
  let rows = fst <$> items
  void $ executeMany conn [sql|
    delete from v2.order_item i
    using (values (?, ?, ?, ?)) as d 
    ( order_group_id
    , order_id
    , household_id
    , product_code
    )
    where i.order_group_id = d.order_group_id
      and i.order_id = d.order_id
      and i.household_id = d.household_id
      and i.product_code = d.product_code
  |] rows

insertOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItemAdjustment)] -> IO ()
insertOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, productCode), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    insert into v2.order_item_adjustment 
    ( new_vat_rate
    , new_price
    , new_quantity
    , is_discontinued
    , "date"
    , order_group_id
    , order_id
    , household_id
    , product_code
    )
    values (?, ?, ?, ?, ?, ?, ?, ?, ?)
  |] rows

updateOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItemAdjustment)] -> IO ()
updateOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, productCode), adj) ->
                    let vatRate = _vatRateType . _priceVatRate . _itemAdjNewPrice $ adj
                        price = _moneyExcVat . _priceAmount . _itemAdjNewPrice $ adj
                        quantity = _itemAdjNewQuantity adj
                        discontinued = _itemAdjIsDiscontinued adj
                        date = _itemAdjDate adj
                    in (vatRate, price, quantity, discontinued, date, groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    update v2.order_item_adjustment a
    set new_vat_rate = u.new_vat_rate
      , new_price = u.new_price
      , new_quantity = u.new_quantity
      , is_discontinued = u.is_discontinued
      , "date" = cast(u."date" as timestamptz)
    from (values (?, ?, ?, ?, ?, ?, ?, ?, ?)) as u
    ( new_vat_rate
    , new_price
    , new_quantity
    , is_discontinued
    , "date"
    , order_group_id
    , order_id
    , household_id
    , product_code
    )
    where a.order_group_id = u.order_group_id 
      and a.order_id = u.order_id 
      and a.household_id = u.household_id
      and a.product_code = u.product_code
  |] rows

deleteOrderItemAdjustments :: Connection -> [((OrderGroupId, OrderId, HouseholdId, ProductCode), OrderItemAdjustment)] -> IO ()
deleteOrderItemAdjustments conn adjustments = do
  let rows = adjustments <&> \((groupId, orderId, householdId, productCode), _) -> (groupId, orderId, householdId, productCode)
  void $ executeMany conn [sql|
    delete from v2.order_item_adjustment a
    using (values (?, ?, ?, ?)) as d 
    ( order_group_id
    , order_id
    , household_id
    , product_code
    )
    where a.order_group_id = d.order_group_id
      and a.order_id = d.order_id
      and a.household_id = d.household_id
      and a.product_code = d.product_code
  |] rows

selectFileUpload :: Connection -> OrderGroupId -> String -> IO [Only ByteString]
selectFileUpload conn groupId fileId = do
  query conn [sql|
    select contents
    from v2.file_upload
    where order_group_id = ?
      and id = ?
  |] (groupId, fileId)

upsertFileUpload :: Connection -> OrderGroupId -> String -> ByteString -> IO ()
upsertFileUpload conn groupId fileId fileContents = do
  void $ execute conn [sql|
    insert into v2.file_upload (order_group_id, id, contents)
    values (?, ?, ?)
    ON CONFLICT (order_group_id, id) DO UPDATE SET contents = EXCLUDED.contents;
  |] (groupId, fileId, Binary fileContents)

deleteFileUpload :: Connection -> OrderGroupId -> String -> IO ()
deleteFileUpload conn groupId fileId = do
  void $ execute conn [sql|
    delete from v2.file_upload
    where order_group_id = ?
      and id = ?
  |] (groupId, fileId)

instance FromRow OrderGroup where
  fromRow = OrderGroup <$> field <*> field <*> field <*> groupSettingsField

groupSettingsField :: RowParser OrderGroupSettings
groupSettingsField = OrderGroupSettings <$> field

instance FromField OrderGroupId where
  fromField f char = OrderGroupId <$> fromField f char

instance ToField OrderGroupId where
  toField = toField . fromOrderGroupId

data HouseholdRow = HouseholdRow
  { householdRow_householdInfo :: HouseholdInfo
  , householdRow_contact :: Contact
  }

instance FromRow HouseholdRow where
  fromRow = HouseholdRow <$> householdInfoField <*> contactField

householdInfoField :: RowParser HouseholdInfo
householdInfoField = HouseholdInfo <$> field <*> field

contactField :: RowParser Contact
contactField = Contact <$> field <*> field <*> field

instance FromField HouseholdId where
  fromField f char = HouseholdId <$> fromField f char

instance ToField HouseholdId where
  toField = toField . fromHouseholdId

data OrderRow = OrderRow
  { orderRow_orderInfo :: OrderInfo
  , orderRow_status :: OrderStatus
  }

instance FromRow OrderRow where
  fromRow = OrderRow <$> orderInfoField <*> orderStatusField

orderInfoField :: RowParser OrderInfo
orderInfoField = do
    orderId  <- field
    orderGroupId  <- field
    created  <- field
    createdById <- field
    createdByName  <- field
    return $ OrderInfo orderId orderGroupId created (createdBy createdById createdByName)
    where
    createdBy (Just id) (Just name) = Just $ HouseholdInfo id name
    createdBy _ _                   = Nothing

orderStatusField :: RowParser OrderStatus
orderStatusField = do
  abandoned <- field
  placed <- field 
  return $ case (abandoned, placed) of
    (True, _) -> OrderAbandoned
    (_, True) -> OrderPlaced
    _ -> OrderOpen

instance FromField OrderId where
  fromField f char = OrderId <$> fromField f char

instance ToField OrderId where
  toField = toField . fromOrderId

data HouseholdOrderRow = HouseholdOrderRow
  { householdOrderRow_orderInfo :: OrderInfo
  , householdOrderRow_orderStatus :: OrderStatus
  , householdOrderRow_householdInfo :: HouseholdInfo
  , householdOrderRow_status :: HouseholdOrderStatus
  }

instance FromRow HouseholdOrderRow where
  fromRow = HouseholdOrderRow <$> orderInfoField <*> orderStatusField <*> householdInfoField <*> householdOrderStatusField

householdOrderStatusField :: RowParser HouseholdOrderStatus
householdOrderStatusField = do
  abandoned <- field
  complete <- field 
  return $ case (abandoned, complete) of
    (True, _) -> HouseholdOrderAbandoned
    (_, True) -> HouseholdOrderComplete
    _ -> HouseholdOrderOpen

data OrderItemRow = OrderItemRow 
  { orderItemRow_product :: Product
  , orderItemRow_quantity :: Int
  , orderItemRow_adjustment :: Maybe OrderItemAdjustment
  , orderItemRow_packed :: Bool
  }

instance FromRow OrderItemRow where
  fromRow = OrderItemRow <$> productField <*> field <*> maybeOrderItemAdjustmentField <*> field

instance FromRow Product where
  fromRow = Product <$> productInfoField <*> productFlagsField

productField :: RowParser Product
productField = Product <$> productInfoField <*> productFlagsField

productInfoField :: RowParser ProductInfo
productInfoField = ProductInfo <$> field <*> field <*> priceField

instance FromField ProductId where
  fromField f char = ProductId <$> fromField f char

instance ToField ProductId where
  toField = toField . fromProductId

instance FromField ProductCode where
  fromField f char = ProductCode . takeWhile (not . isSpace) <$> fromField f char

instance ToField ProductCode where
  toField = toField . {- padChar10 . -} fromProductCode
 
padChar10 :: String -> String
padChar10 s = take 10 $ s ++ repeat ' '

productFlagsField :: RowParser ProductFlags
productFlagsField = ProductFlags <$> field <*> field <*> field <*> field <*> field <*> field

priceField :: RowParser Price
priceField = atVatRate <$> vatRateField <*> field

instance ToField Price where
  toField p = Many [ toField . _vatRateType . _priceVatRate $ p
                   , toField (realToFrac . _moneyExcVat . _priceAmount $ p :: Double)
                   ]

vatRateField :: RowParser VatRate
vatRateField = VatRate <$> field <*> field

maybeOrderItemAdjustmentField :: RowParser (Maybe OrderItemAdjustment)
maybeOrderItemAdjustmentField = do
  newPrice <- maybePriceField
  newQuantity <- field
  isDiscontinued <- field
  date <- field
  case (newPrice, newQuantity, isDiscontinued, date) of
    (Just p, Just q, Just disc, Just date) -> return $ Just $ OrderItemAdjustment p q disc date
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

instance FromRow VatRate where
  fromRow = VatRate <$> field <*> field

instance ToRow ProductCatalogueEntry where
  toRow e = [ toField $ _catalogueEntryCode e
            , toField $ _catalogueEntryCategory e
            , toField $ _catalogueEntryBrand e
            , toField $ _catalogueEntryDescription e
            , toField $ _catalogueEntryText e
            , toField $ _catalogueEntrySize e
            , toField $ _vatRateType . _priceVatRate . _catalogueEntryPrice $ e
            , toField $ _moneyExcVat . _priceAmount . _catalogueEntryPrice $ e
            , toField $ _catalogueEntryRrp e
            , toField $ _catalogueEntryBiodynamic e
            , toField $ _catalogueEntryFairTrade e
            , toField $ _catalogueEntryGlutenFree e
            , toField $ _catalogueEntryOrganic e
            , toField $ _catalogueEntryAddedSugar e
            , toField $ _catalogueEntryVegan e
            , toField $ _catalogueEntryUpdated e
            ]

instance FromRow ProductCatalogueEntry where
  fromRow = ProductCatalogueEntry <$> field <*> field <*> field <*> field <*> field <*> field <*> priceField <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data UpdateOrderItem = UpdateOrderItem
  { updateOrderItem_quantity :: Int
  , updateOrderItem_product_name :: String
  , updateOrderItem_product_vat_rate :: VatRateType
  , updateOrderItem_product_vat_rate_multiplier :: Double
  , updateOrderItem_product_price :: Int
  , updateOrderItem_product_is_biodynamic :: Bool
  , updateOrderItem_product_is_fair_trade :: Bool
  , updateOrderItem_product_is_gluten_free :: Bool
  , updateOrderItem_product_is_organic :: Bool
  , updateOrderItem_product_is_added_sugar :: Bool
  , updateOrderItem_product_is_vegan :: Bool
  , updateOrderItem_order_group_id :: OrderGroupId
  , updateOrderItem_order_id :: OrderId
  , updateOrderItem_household_id :: HouseholdId
  , updateOrderItem_product_code :: ProductCode
  , updateOrderItem_is_packed :: Bool
  } deriving (Show)

instance ToRow UpdateOrderItem where
  toRow i = [ toField $ updateOrderItem_quantity i
            , toField $ updateOrderItem_product_name i
            , toField $ updateOrderItem_product_vat_rate i
            , toField $ updateOrderItem_product_vat_rate_multiplier i
            , toField $ updateOrderItem_product_price i
            , toField $ updateOrderItem_product_is_biodynamic i
            , toField $ updateOrderItem_product_is_fair_trade i
            , toField $ updateOrderItem_product_is_gluten_free i
            , toField $ updateOrderItem_product_is_organic i
            , toField $ updateOrderItem_product_is_added_sugar i
            , toField $ updateOrderItem_product_is_vegan i
            , toField $ updateOrderItem_order_group_id i
            , toField $ updateOrderItem_order_id i
            , toField $ updateOrderItem_household_id i
            , toField $ updateOrderItem_product_code i
            , toField $ updateOrderItem_is_packed i
            ]

data WhereParam = ForOrderGroup OrderGroupId
                | ForOrder OrderId
                | ForHousehold HouseholdId
                | ForProductId ProductId
                | ForProductCode ProductCode
                | ForPayment PaymentId
                | OrderIsCurrent
                | OrderIsPast

hasParam :: WhereParam -> Bool
hasParam OrderIsCurrent = False
hasParam OrderIsPast = False
hasParam _ = True

instance ToField WhereParam where
  toField (ForOrderGroup a) = toField a
  toField (ForOrder a) = toField a
  toField (ForHousehold a) = toField a
  toField (ForProductId a) = toField a
  toField (ForProductCode a) = toField a
  toField (ForPayment a) = toField a
  -- Never used but need to handle the case
  toField _ = Plain "null"

toWhereClause :: [WhereParam] -> (WhereParam -> Maybe Query) -> (Query, [WhereParam])
toWhereClause params fn = foldl' fn' ([sql| |], []) params
  where
    fn' (q, p) wp = case fn wp of
                     Just wq -> (q <> " and (" <> wq <> ") ", p <> if hasParam wp then [wp] else [])
                     _ -> (q, p)

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]