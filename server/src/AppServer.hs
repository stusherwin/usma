{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AppServer (appServer) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack, toStrict)
import Data.Csv (ToNamedRecord(..), (.=), namedRecord, encodeByName)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V (fromList)
import Servant
import Servant.Multipart
import Data.UUID (toString)
import Data.UUID.V1 (nextUUID)

import Types
import AppApi
import qualified Database as D
import Config
import ProductCatalogueImport
import ReconcileHouseholdOrderFile
import ProductImage (fetchProductImage)

appServer :: Config -> Text -> Server AppApi
appServer config groupKey = 
       queryServer config groupKey
  :<|> commandServer config groupKey

queryServer :: Config -> Text -> Server QueryApi
queryServer config groupKey = 
       allData
  :<|> productCatalogueData
  :<|> collectiveOrder
  :<|> pastCollectiveOrders
  :<|> householdOrders
  :<|> pastHouseholdOrders
  :<|> households
  :<|> householdPayments
  :<|> productCatalogue
  :<|> productImage
  :<|> collectiveOrderDownload
  :<|> householdOrdersDownload
  :<|> pastCollectiveOrderDownload
  :<|> pastHouseholdOrdersDownload
  :<|> productCatalogueCategories
  :<|> productCatalogueBrands
  :<|> groupSettings
  where
  conn = connectionString config
  
  allData :: Handler ApiData
  allData = findGroupOr404 conn groupKey $ \groupId -> do
    collectiveOrder <- liftIO $ D.getCollectiveOrder conn groupId
    pastCollectiveOrders <- liftIO $ D.getPastCollectiveOrders conn groupId
    householdOrders <- liftIO $ D.getHouseholdOrders conn groupId
    pastHouseholdOrders <- liftIO $ D.getPastHouseholdOrders conn groupId
    households <- liftIO $ D.getHouseholds conn groupId
    householdPayments <- liftIO $ D.getHouseholdPayments conn groupId
    groupSettings <- liftIO $ D.getGroupSettings conn groupId
    return $ ApiData collectiveOrder pastCollectiveOrders householdOrders pastHouseholdOrders households householdPayments groupSettings

  productCatalogueData :: Handler ProductCatalogueApiData
  productCatalogueData = findGroupOr404 conn groupKey $ \_ -> do
    productCatalogue <- liftIO $ D.getProductCatalogue conn
    categories <- liftIO $ D.getProductCatalogueCategories conn
    brands <- liftIO $ D.getProductCatalogueBrands conn
    return $ ProductCatalogueApiData productCatalogue categories brands

  collectiveOrder :: Handler (Maybe CollectiveOrder)
  collectiveOrder = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getCollectiveOrder conn groupId
  
  pastCollectiveOrders :: Handler [PastCollectiveOrder]
  pastCollectiveOrders = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getPastCollectiveOrders conn groupId
  
  householdOrders :: Handler [HouseholdOrder]
  householdOrders = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getHouseholdOrders conn groupId
  
  pastHouseholdOrders :: Handler [PastHouseholdOrder]
  pastHouseholdOrders = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getPastHouseholdOrders conn groupId

  households :: Handler [Household]
  households = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getHouseholds conn groupId

  householdPayments :: Handler [HouseholdPayment]
  householdPayments = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.getHouseholdPayments conn groupId

  productCatalogue :: Handler [ProductCatalogueEntry]
  productCatalogue = liftIO $ D.getProductCatalogue conn

  productImage :: String -> Handler BL.ByteString
  productImage code = do
    image <- liftIO $ fetchProductImage conn code
    case image of
      Just i -> return i
      _ -> throwError err404

  collectiveOrderDownload :: Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  collectiveOrderDownload = findGroupOr404 conn groupKey $ \groupId -> do
    order <- liftIO $ D.getCollectiveOrder conn groupId
    let items = case order of
                  Nothing -> []
                  Just o -> map toCsvItem $ coItems o
    return $ addHeader "attachment; filename=\"order.csv\"" $ encodeByName (V.fromList ["Code", "Product", "Price", "Quantity", "Total"]) items
    where
    toCsvItem (OrderItem { oiProductName = name
                         , oiProductCode = code
                         , oiAdjustment = Just (OrderItemAdjustment { 
                            oiaOldProductPriceExcVat = price
                            , oiaOldItemQuantity = qty
                            , oiaOldItemTotalExcVat = total
                           })
                         }) = CsvItem name code price qty total ""
    toCsvItem (OrderItem { oiProductName = name
                         , oiProductCode = code
                         , oiProductPriceExcVat = price
                         , oiItemQuantity = qty
                         , oiItemTotalExcVat = total
                         }) = CsvItem name code price qty total ""

  householdOrdersDownload :: Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  householdOrdersDownload = findGroupOr404 conn groupKey $ \groupId -> do
    order <- liftIO $ D.getCollectiveOrder conn groupId
    case order of
      Nothing -> throwError err404
      Just o -> do
        householdOrders <- liftIO $ D.getHouseholdOrders conn groupId
        let items = (map toCsvItem) . concat . withHouseholdName . (forOrder o) $ householdOrders
        return $ addHeader "attachment; filename=\"order.csv\"" $ encodeByName (V.fromList ["Code", "Product", "Price", "Quantity", "Total", "Reference"]) items
    where
    forOrder o = filter ((== coId o) . hoOrderId)
    withHouseholdName = map (\(HouseholdOrder { hoHouseholdName = n, hoItems = is }) -> map (n,) is)
    toCsvItem (householdName,OrderItem { oiProductName = name
                                       , oiProductCode = code
                                       , oiAdjustment = Just (OrderItemAdjustment { 
                                          oiaOldProductPriceExcVat = price
                                          , oiaOldItemQuantity = qty
                                          , oiaOldItemTotalExcVat = total
                                         })
                                       }) = CsvItem name code price qty total householdName
    toCsvItem (householdName, OrderItem { oiProductName = name
                                        , oiProductCode = code
                                        , oiProductPriceExcVat = price
                                        , oiItemQuantity = qty
                                        , oiItemTotalExcVat = total
                                        }) = CsvItem name code price qty total householdName

  pastCollectiveOrderDownload :: Int -> Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  pastCollectiveOrderDownload orderId = findGroupOr404 conn groupKey $ \groupId -> do
    orders <- liftIO $ D.getPastCollectiveOrders conn groupId
    let order = find ((== orderId) . pcoId) orders
    let items = case order of
                  Nothing -> []
                  Just o -> map toCsvItem $ pcoItems o
    return $ addHeader "attachment; filename=\"order.csv\"" $ encodeByName (V.fromList ["Code", "Product", "Price", "Quantity", "Total"]) items
    where
    toCsvItem (OrderItem { oiProductName = name
                         , oiProductCode = code
                         , oiAdjustment = Just (OrderItemAdjustment { 
                            oiaOldProductPriceExcVat = price
                            , oiaOldItemQuantity = qty
                            , oiaOldItemTotalExcVat = total
                           })
                         }) = CsvItem name code price qty total ""
    toCsvItem (OrderItem { oiProductName = name
                         , oiProductCode = code
                         , oiProductPriceExcVat = price
                         , oiItemQuantity = qty
                         , oiItemTotalExcVat = total
                         }) = CsvItem name code price qty total ""

  pastHouseholdOrdersDownload :: Int -> Handler (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  pastHouseholdOrdersDownload orderId = findGroupOr404 conn groupKey $ \groupId -> do
    householdOrders <- liftIO $ D.getPastHouseholdOrders conn groupId
    let items = (map toCsvItem) . concat . withHouseholdName . forOrder $ householdOrders
    return $ addHeader "attachment; filename=\"order.csv\"" $ encodeByName (V.fromList ["Code", "Product", "Price", "Quantity", "Total", "Reference"]) items
    where
    forOrder = filter ((== orderId) . phoOrderId)
    withHouseholdName = map (\(PastHouseholdOrder { phoHouseholdName = n, phoItems = is }) -> map (n,) is)
    toCsvItem (householdName,OrderItem { oiProductName = name
                                       , oiProductCode = code
                                       , oiAdjustment = Just (OrderItemAdjustment { 
                                          oiaOldProductPriceExcVat = price
                                          , oiaOldItemQuantity = qty
                                          , oiaOldItemTotalExcVat = total
                                         })
                                       }) = CsvItem name code price qty total householdName
    toCsvItem (householdName, OrderItem { oiProductName = name
                                        , oiProductCode = code
                                        , oiProductPriceExcVat = price
                                        , oiItemQuantity = qty
                                        , oiItemTotalExcVat = total
                                        }) = CsvItem name code price qty total householdName

  productCatalogueCategories :: Handler [String]
  productCatalogueCategories = liftIO $ D.getProductCatalogueCategories conn

  productCatalogueBrands :: Handler [String]
  productCatalogueBrands = liftIO $ D.getProductCatalogueBrands conn

  groupSettings :: Handler GroupSettings
  groupSettings = findGroupOr404 conn groupKey $ \groupId -> liftIO $ D.getGroupSettings conn groupId

commandServer :: Config -> Text -> Server CommandApi
commandServer config groupKey = 
       createOrderForHousehold groupKey
  :<|> createOrder groupKey
  :<|> placeOrder groupKey
  :<|> abandonOrder groupKey
  :<|> abandonHouseholdOrder groupKey
  :<|> completeHouseholdOrder groupKey
  :<|> reopenHouseholdOrder groupKey
  :<|> ensureHouseholdOrderItem groupKey
  :<|> ensureAllItemsFromPastHouseholdOrder groupKey
  :<|> removeHouseholdOrderItem groupKey
  :<|> createHousehold groupKey
  :<|> updateHousehold groupKey
  :<|> archiveHousehold groupKey
  :<|> createHouseholdPayment groupKey
  :<|> updateHouseholdPayment groupKey
  :<|> archiveHouseholdPayment groupKey
  :<|> uploadProductCatalogue
  :<|> acceptCatalogueUpdates groupKey
  :<|> reconcileOrderItem groupKey
  :<|> uploadOrderFile
  :<|> reconcileHouseholdOrderFromFile groupKey

  where
  conn = connectionString config
  
  createOrderForHousehold :: Text -> Int -> Handler Int
  createOrderForHousehold groupKey householdId = findGroupOr404 conn groupKey $ \groupId -> do
    date <- liftIO $ getCurrentTime
    liftIO $ D.createOrder conn groupId date (Just householdId)

  createOrder :: Text -> Handler Int
  createOrder groupKey = findGroupOr404 conn groupKey $ \groupId -> do
    date <- liftIO $ getCurrentTime
    liftIO $ D.createOrder conn groupId date Nothing

  placeOrder :: Text -> Int -> Handler ()
  placeOrder groupKey orderId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.closeOrder conn groupId False orderId

  abandonOrder :: Text -> Int -> Handler ()
  abandonOrder groupKey orderId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.closeOrder conn groupId True orderId

  abandonHouseholdOrder :: Text -> Int -> Int -> Handler ()
  abandonHouseholdOrder groupKey orderId householdId = findGroupOr404 conn groupKey $ \groupId -> do
    liftIO $ D.cancelHouseholdOrder conn groupId orderId householdId
    householdOrders <- liftIO $ D.getHouseholdOrders conn groupId
    when (all hoIsAbandoned householdOrders) $ do
      liftIO $ D.closeOrder conn groupId True orderId

  completeHouseholdOrder :: Text -> Int -> Int -> Handler ()
  completeHouseholdOrder groupKey orderId householdId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.completeHouseholdOrder conn groupId orderId householdId

  reopenHouseholdOrder :: Text -> Int -> Int -> Handler ()
  reopenHouseholdOrder groupKey orderId householdId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.reopenHouseholdOrder conn groupId orderId householdId

  ensureHouseholdOrderItem :: Text -> Int -> Int -> String -> HouseholdOrderItemDetails -> Handler ()
  ensureHouseholdOrderItem groupKey orderId householdId productCode details = findGroupOr404 conn groupKey $ \groupId -> do
    date <- liftIO $ getCurrentTime
    liftIO $ D.ensureHouseholdOrderItem conn groupId orderId householdId productCode date details

  ensureAllItemsFromPastHouseholdOrder :: Text -> Int -> Int -> Int -> Handler ()
  ensureAllItemsFromPastHouseholdOrder groupKey orderId householdId pastOrderId = findGroupOr404 conn groupKey $ \groupId -> do
    date <- liftIO $ getCurrentTime
    liftIO $ D.ensureAllItemsFromPastHouseholdOrder conn groupId orderId householdId pastOrderId date

  removeHouseholdOrderItem :: Text -> Int -> Int -> Int -> Handler ()
  removeHouseholdOrderItem groupKey orderId householdId productId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.removeHouseholdOrderItem conn groupId orderId householdId productId

  createHousehold :: Text -> HouseholdDetails -> Handler Int
  createHousehold groupKey details = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.createHousehold conn groupId details

  updateHousehold :: Text -> Int -> HouseholdDetails -> Handler ()
  updateHousehold groupKey householdId details = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.updateHousehold conn groupId householdId details

  archiveHousehold :: Text -> Int -> Handler ()
  archiveHousehold groupKey householdId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.archiveHousehold conn groupId householdId

  createHouseholdPayment :: Text -> Int -> HouseholdPaymentDetails -> Handler Int
  createHouseholdPayment groupKey householdId details = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.createHouseholdPayment conn groupId householdId details

  updateHouseholdPayment :: Text -> Int -> HouseholdPaymentDetails -> Handler ()
  updateHouseholdPayment groupKey householdPaymentId details = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.updateHouseholdPayment conn groupId householdPaymentId details

  archiveHouseholdPayment :: Text -> Int -> Handler ()
  archiveHouseholdPayment groupKey householdPaymentId = findGroupOr404 conn groupKey $ \groupId ->
    liftIO $ D.archiveHouseholdPayment conn groupId householdPaymentId

  uploadProductCatalogue :: MultipartData Mem -> Handler ()
  uploadProductCatalogue multipartData = do
    when (length (files multipartData) /= 1) $
      throwError err400
    let file = (files multipartData) !! 0
    date <- liftIO $ getCurrentTime
    liftIO $ importProductCatalogue conn date $ BL.unpack $ fdPayload file

  acceptCatalogueUpdates :: Text -> Int -> Int -> Handler ()
  acceptCatalogueUpdates groupKey orderId householdId = findGroupOr404 conn groupKey $ \groupId -> do
    date <- liftIO $ getCurrentTime
    liftIO $ D.acceptCatalogueUpdates conn groupId date orderId householdId

  reconcileOrderItem :: Text -> Int -> Int -> ReconcileOrderItemDetails -> Handler ()
  reconcileOrderItem groupKey orderId productId details = findGroupOr404 conn groupKey $ \groupId -> do
    liftIO $ D.reconcileOrderItem conn groupId orderId productId details

  uploadOrderFile :: MultipartData Mem -> Handler (Headers '[Header "Cache-Control" String] (Maybe UploadedOrderFile))
  uploadOrderFile multipartData = do
    when (length (files multipartData) /= 1) $
      throwError err400
    let file = (files multipartData) !! 0
    orderFileDetails <- liftIO $ uploadOrderFile' file
    return $ addHeader "no-cache" orderFileDetails

  uploadOrderFile' :: FileData Mem -> IO (Maybe UploadedOrderFile)
  uploadOrderFile' file = do
    let fileContents = fdPayload file
    uuid <- liftIO nextUUID
    case uuid of
      Just id -> do
        let fileId = toString id
        D.saveUploadedOrderFile conn fileId $ BL.toStrict fileContents
        return $ getHouseholdOrderFileDetails fileId $ BL.unpack fileContents
      _ -> return Nothing

  reconcileHouseholdOrderFromFile :: Text -> Int -> Int -> String -> Handler ()
  reconcileHouseholdOrderFromFile groupKey orderId householdId fileId = findGroupOr404 conn groupKey $ \groupId -> do
    fileContents <- liftIO $ D.getUploadedOrderFile conn fileId
    case fileContents of
      Just contents -> do
        liftIO $ reconcileHouseholdOrderFile conn groupId orderId householdId $ B.unpack contents
        liftIO $ D.deleteUploadedOrderFile conn fileId
      _ -> return ()

findGroup :: B.ByteString -> Text -> IO (Maybe Int)
findGroup conn groupKey = do
  rotaId <- D.getGroup conn (T.unpack groupKey)
  return rotaId

findGroupOr404 :: B.ByteString -> Text -> (Int -> Handler a) -> Handler a
findGroupOr404 conn groupKey handler = do
  groupId <- liftIO $ findGroup conn groupKey
  case groupId of
    Just id -> handler id
    _ -> throwError err404

data CsvItem = CsvItem 
  { csvName :: String
  , csvCode :: String
  , csvPrice :: Int
  , csvQuantity :: Int
  , csvTotal :: Int
  , csvReference :: String
  }

instance ToNamedRecord CsvItem where
  toNamedRecord (CsvItem 
    { csvName = name
    , csvCode = code
    , csvPrice = price
    , csvQuantity = qty
    , csvTotal = tot
    , csvReference = ref
    }) 
    = namedRecord 
      [ "Product" .= name
      , "Code" .= code
      , "Price" .= price'
      , "Quantity" .= qty
      , "Total" .= total' 
      , "Reference" .= ref 
      ]
    where
    price' = ((fromIntegral price) :: Double) / 100.0
    total' = ((fromIntegral tot) :: Double) / 100.0