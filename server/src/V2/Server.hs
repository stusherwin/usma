{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module V2.Server (server) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack, toStrict)
import qualified Data.Map as M (elems)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime)
import           Data.UUID (toString)
import           Data.UUID.V1 (nextUUID)
import           Safe (headMay)
import           Servant
import           Servant.Multipart (MultipartData(..), FileData(..), Mem)

import qualified V2.Api as Api
import           Config (Config(..))
import           V2.CsvExport (exportOrderItems, exportOrderItemsByHousehold)
import           V2.Domain
import           V2.Repository as R
import           V2.SumaCatalogue (fetchProductImage, fetchProductImageFull, fetchProductCatalogue)
import           V2.ReconcileSumaOrderFile (parseOrderFileDetails, parseOrderFileUpdates)

server :: Config -> Text -> Server Api.Api
server config groupKey =
         queryServer repoConfig
    :<|> commandServer repoConfig
  where
    repoConfig = RepositoryConfig (connectionStringV2 config) (T.unpack groupKey)

queryServer :: RepositoryConfig -> Server Api.QueryApi
queryServer config = 
         allData
    :<|> productCatalogueData     
    :<|> collectiveOrder
    :<|> pastCollectiveOrders
    :<|> householdOrders
    :<|> pastHouseholdOrders
    :<|> households
    :<|> householdPayments
    :<|> productImage
    :<|> productImageFull
    :<|> collectiveOrderDownload
    :<|> householdOrdersDownload
    :<|> pastCollectiveOrderDownload
    :<|> pastHouseholdOrdersDownload
    :<|> groupSettings
  where
    allData :: Handler Api.ApiData
    allData = withRepository config $ \(repo, groupId) -> do
      currentProductIds <- liftIO $ getProductIdsForCurrentOrder repo groupId
      pastProductIds <- liftIO $ getProductIdsForPastOrders repo groupId

      collectiveOrder <- liftIO $ getCurrentOrder repo groupId
      let collectiveOrder' = apiCollectiveOrder currentProductIds <$> collectiveOrder

      pastCollectiveOrders <- liftIO $ getPastOrders repo (Just groupId)
      let pastCollectiveOrders' = apiPastCollectiveOrder pastProductIds <$> pastCollectiveOrders

      householdOrders <- liftIO $ getCurrentHouseholdOrders repo (Just groupId)
      let householdOrders' = apiHouseholdOrder currentProductIds <$> householdOrders

      pastHouseholdOrders <- liftIO $ getPastHouseholdOrders repo (Just groupId)
      let pastHouseholdOrders' = apiPastHouseholdOrder pastProductIds <$> pastHouseholdOrders

      households <- liftIO $ getHouseholds repo (Just groupId)
      let households' = apiHousehold <$> households

      householdPayments <- liftIO $ getPayments repo (Just groupId)
      let householdPayments' = apiHouseholdPayment <$> householdPayments

      groupSettings <- liftIO $ getOrderGroup repo groupId
      let groupSettings' = apiGroupSettings groupSettings

      return $ Api.ApiData collectiveOrder' 
                           pastCollectiveOrders'
                           householdOrders' 
                           pastHouseholdOrders' 
                           households' 
                           householdPayments' 
                           groupSettings'

    productCatalogueData :: Handler Api.ProductCatalogueApiData
    productCatalogueData = withRepository config $ \(repo, _) -> do
      catalogue <- MaybeT $ liftIO $ fetchProductCatalogue repo
      let productCatalogue = map apiProductCatalogueEntry $ entries $ catalogue
      let cs = categories catalogue
      let bs = brands catalogue
    
      return $ Api.ProductCatalogueApiData productCatalogue cs bs

    collectiveOrder :: Handler (Maybe Api.CollectiveOrder)
    collectiveOrder = withRepository config $ \(repo, groupId) -> do
      order <- liftIO $ getCurrentOrder repo groupId
      productIds <- liftIO $ getProductIdsForCurrentOrder repo groupId
      return $ apiCollectiveOrder productIds <$> order

    pastCollectiveOrders :: Handler [Api.PastCollectiveOrder]
    pastCollectiveOrders = withRepository config $ \(repo, groupId) -> do
      orders <- liftIO $ getPastOrders repo (Just groupId)
      productIds <- liftIO $ getProductIdsForPastOrders repo groupId
      return $ apiPastCollectiveOrder productIds <$> orders

    householdOrders :: Handler [Api.HouseholdOrder]
    householdOrders = withRepository config $ \(repo, groupId) -> do
      orders <- liftIO $ getCurrentHouseholdOrders repo (Just groupId)
      productIds <- liftIO $ getProductIdsForCurrentOrder repo groupId
      return $ apiHouseholdOrder productIds <$> orders

    pastHouseholdOrders :: Handler [Api.PastHouseholdOrder]
    pastHouseholdOrders = withRepository config $ \(repo, groupId) -> do
      orders <- liftIO $ getPastHouseholdOrders repo (Just groupId)
      productIds <- liftIO $ getProductIdsForPastOrders repo groupId
      return $ apiPastHouseholdOrder productIds <$> orders

    households :: Handler [Api.Household]
    households = withRepository config $ \(repo, groupId) -> do
      households <- liftIO $ getHouseholds repo (Just groupId)
      return $ apiHousehold <$> households

    householdPayments :: Handler [Api.HouseholdPayment]
    householdPayments = withRepository config $ \(repo, groupId) -> do
      payments <- liftIO $ getPayments repo (Just groupId)
      return $ apiHouseholdPayment <$> payments

    productImage :: String -> Handler BL.ByteString
    productImage code = withRepository config $ \(repo, _) -> do
      MaybeT $ liftIO $ fetchProductImage repo code

    productImageFull :: String -> Handler BL.ByteString
    productImageFull code = withRepository config $ \(repo, _) -> do
      MaybeT $ liftIO $ fetchProductImageFull repo code

    collectiveOrderDownload :: Handler Api.FileDownload
    collectiveOrderDownload = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getCurrentOrder repo groupId
        return $ fileDownload "order.csv" $ exportOrderItems order

    householdOrdersDownload :: Handler Api.FileDownload
    householdOrdersDownload = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getCurrentOrder repo groupId
        return $ fileDownload "order.csv" $ exportOrderItemsByHousehold order

    pastCollectiveOrderDownload :: Int -> Handler Api.FileDownload
    pastCollectiveOrderDownload orderId = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getOrder repo groupId (OrderId orderId)
        return $ fileDownload "order.csv" $ exportOrderItems order

    pastHouseholdOrdersDownload :: Int -> Handler Api.FileDownload
    pastHouseholdOrdersDownload orderId = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getOrder repo groupId (OrderId orderId)
        return $ fileDownload "order.csv" $ exportOrderItemsByHousehold order

    groupSettings :: Handler Api.GroupSettings
    groupSettings = withRepository config $ \(repo, groupId) -> do
      group <- liftIO $ getOrderGroup repo groupId
      return $ apiGroupSettings group

commandServer :: RepositoryConfig -> Server Api.CommandApi
commandServer config  = 
         createOrderForHousehold
    :<|> createOrder
    :<|> placeOrder
    :<|> abandonOrder
    :<|> abandonHouseholdOrder
    :<|> completeHouseholdOrder
    :<|> reopenHouseholdOrder
    :<|> ensureHouseholdOrderItem
    :<|> ensureAllItemsFromPastHouseholdOrder
    :<|> removeHouseholdOrderItem
    :<|> createHousehold
    :<|> updateHousehold
    :<|> archiveHousehold
    :<|> createHouseholdPayment
    :<|> updateHouseholdPayment
    :<|> archiveHouseholdPayment
    :<|> uploadProductCatalogue
    :<|> acceptCatalogueUpdates
    :<|> reconcileOrderItem
    :<|> uploadOrderFile
    :<|> reconcileHouseholdOrderFromFile
    :<|> toggleItemPacked
  where
    createOrderForHousehold :: Int -> Handler Int
    createOrderForHousehold householdId = withRepository config $ \(repo, groupId) -> do
      date <- liftIO $ getCurrentTime
      orderId <- liftIO $ R.createOrder repo groupId $ OrderSpec date (Just $ HouseholdId householdId)
      return $ fromOrderId orderId

    createOrder :: Handler Int
    createOrder = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      orderId <- liftIO $ R.createOrder repo groupId $ OrderSpec date Nothing
      return $ fromOrderId orderId

    abandonOrder :: Int -> Handler ()
    abandonOrder orderId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.abandonOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    placeOrder :: Int -> Handler ()
    placeOrder orderId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.placeOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    abandonHouseholdOrder :: Int -> Int -> Handler ()
    abandonHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.abandonHouseholdOrder (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.completeHouseholdOrder (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    reopenHouseholdOrder :: Int -> Int -> Handler ()  
    reopenHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      catalogue <- MaybeT $ liftIO $ fetchProductCatalogue repo
      let order' = applyCatalogueUpdate catalogue . V2.Domain.reopenHouseholdOrder (HouseholdId householdId) $ order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    ensureHouseholdOrderItem :: Int -> Int -> String -> Api.HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem orderId householdId productCode details = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      household <- MaybeT $ getHouseholdInfo repo groupId (HouseholdId householdId)
      catalogue <- MaybeT $ liftIO $ fetchProductCatalogue repo
      let order' = addOrUpdateOrderItems catalogue household [(ProductCode productCode, Api.hoidetQuantity details)] order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    ensureAllItemsFromPastHouseholdOrder :: Int -> Int -> Int -> Handler ()
    ensureAllItemsFromPastHouseholdOrder orderId householdId pastOrderId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      pastOrder <- MaybeT $ getOrder repo groupId (OrderId pastOrderId)
      household <- MaybeT $ getHouseholdInfo repo groupId (HouseholdId householdId)
      (date, fileContents) <- MaybeT $ liftIO $ getProductCatalogueFile repo
      vatRates <- liftIO $ getVatRates repo
      let catalogue = parseCatalogue vatRates date $ T.unpack fileContents
      let order' = addOrderItemsFromPastOrder catalogue household pastOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem orderId householdId productId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      productCode <- MaybeT $ getProductCode repo (ProductId productId)
      let order' = V2.Domain.removeOrderItem (HouseholdId householdId) productCode order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    createHousehold :: Api.HouseholdDetails -> Handler Int
    createHousehold details = withRepository config $ \(repo, groupId) -> do
        householdId <- liftIO $ R.createHousehold repo groupId spec
        return $ fromHouseholdId householdId
      where
        contact = Contact (Api.hdetContactName details)
                          (Api.hdetContactEmail details)
                          (Api.hdetContactPhone details)
        spec = HouseholdSpec (Api.hdetName details) contact

    updateHousehold :: Int -> Api.HouseholdDetails -> Handler ()
    updateHousehold householdId details = withRepository config $ \(repo, groupId) -> do
        household <- MaybeT $ getHousehold repo groupId (HouseholdId householdId)
        let household' = V2.Domain.updateHousehold name contact household
        liftIO $ setHousehold repo groupId (household, household')
      where
        name = Api.hdetName details
        contact = Contact (Api.hdetContactName details)
                          (Api.hdetContactEmail details)
                          (Api.hdetContactPhone details)

    archiveHousehold :: Int -> Handler ()
    archiveHousehold householdId = withRepository config $ \(repo, groupId) ->
      liftIO $ removeHousehold repo groupId (HouseholdId householdId)

    createHouseholdPayment :: Int -> Api.HouseholdPaymentDetails -> Handler Int
    createHouseholdPayment householdId details = withRepository config $ \(repo, groupId) -> do
        paymentId <- liftIO $ createPayment repo groupId $ PaymentSpec (HouseholdId householdId) date amount
        return $ fromPaymentId paymentId
      where
        date = UTCTime (Api.hpdetDate details) (secondsToDiffTime 0)
        amount = Api.hpdetAmount details

    updateHouseholdPayment :: Int -> Api.HouseholdPaymentDetails -> Handler ()
    updateHouseholdPayment paymentId details = withRepository config $ \(repo, groupId) -> do
        payment <- MaybeT $ getPayment repo groupId (PaymentId paymentId)
        let payment' = updatePayment date amount payment
        liftIO $ setPayment repo groupId (payment, payment')
      where
        date = UTCTime (Api.hpdetDate details) (secondsToDiffTime 0)
        amount = Api.hpdetAmount details
    
    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment paymentId = withRepository config $ \(repo, groupId) ->
      liftIO $ removePayment repo groupId (PaymentId paymentId)

    uploadProductCatalogue :: MultipartData Mem -> Handler ()
    uploadProductCatalogue multipartData = withRepository config $ \(repo, _) -> do
      date <- liftIO getCurrentTime
      fileContents <- uploadSingleFile multipartData
      liftIO $ setProductCatalogueFile repo date $ T.pack $ BL.unpack fileContents
      vatRates <- liftIO $ getVatRates repo
      orders <- liftIO $ getCurrentOrders repo Nothing
      let catalogue = parseCatalogue vatRates date $ BL.unpack fileContents
      let orders' = map (applyCatalogueUpdate catalogue) orders
      liftIO $ setOrders repo (orders, orders')
      return ()

    acceptCatalogueUpdates :: Int -> Int -> Handler ()
    acceptCatalogueUpdates orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.acceptCatalogueUpdate (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    reconcileOrderItem :: Int -> Int -> Api.ReconcileOrderItemDetails -> Handler ()
    reconcileOrderItem orderId productId details = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      productCode <- MaybeT $ getProductCode repo (ProductId productId)
      let updates = map (\h -> (HouseholdId $ Api.hqdetHouseholdId h, OrderItemSpec productCode (Api.roidetProductPriceExcVat details) (Api.hqdetItemQuantity h)))
                  $ Api.roidetHouseholdQuantities details
      let order' = reconcileOrderItems date updates order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    uploadOrderFile :: MultipartData Mem -> Handler (Headers '[Header "Cache-Control" String] (Maybe Api.UploadedOrderFile))
    uploadOrderFile multipartData = withRepository config $ \(repo, groupId) -> do
      fileContents <- uploadSingleFile multipartData
      uuid <- MaybeT $ liftIO nextUUID
      let fileId = toString uuid
      liftIO $ setFileUpload repo groupId fileId $ BL.toStrict fileContents
      let orderFile = parseOrderFileDetails fileId $ BL.unpack fileContents
      return $ addHeader "no-cache" orderFile

    reconcileHouseholdOrderFromFile :: Int -> Int -> String -> Handler ()
    reconcileHouseholdOrderFromFile orderId householdId fileId = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      fileContents <- MaybeT $ liftIO $ getFileUpload repo groupId fileId
      let updates = map (HouseholdId householdId,)
                  $ parseOrderFileUpdates $ B.unpack fileContents
      let order' = reconcileOrderItems date updates order
      liftIO $ setOrders repo ([order], [order'])
      liftIO $ removeFileUpload repo groupId fileId

    toggleItemPacked :: Int -> Int -> String -> Handler ()
    toggleItemPacked orderId householdId productCode = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = V2.Domain.updateOrderItem (V2.Domain.toggleItemIsPacked) (HouseholdId householdId) (ProductCode productCode) order
      liftIO $ setOrders repo ([order], [order'])
      return ()


uploadSingleFile :: MultipartData Mem -> MaybeT IO BL.ByteString
uploadSingleFile multipartData = do
  file <- MaybeT $ return $ headMay $ files multipartData
  return $ fdPayload file

fileDownload :: String -> BL.ByteString -> Api.FileDownload
fileDownload fileName file = addHeader header file
  where
    header = T.pack $ "attachment; filename=\"" ++ fileName ++ "\""

withRepository :: RepositoryConfig -> ((Repository, OrderGroupId) -> MaybeT IO a) -> Handler a
withRepository config query = do
  result <- liftIO $ runMaybeT $ connect config $ query
  case result of
    Just r -> return r
    _ -> throwError err404

apiHousehold :: V2.Domain.Household -> Api.Household 
apiHousehold h = Api.Household
  { Api.hId            = fromHouseholdId . _householdId . _householdInfo $ h
  , Api.hName          = _householdName                 . _householdInfo $ h
  , Api.hContactName   = _contactName  . _householdContact $ h
  , Api.hContactEmail  = _contactEmail . _householdContact $ h
  , Api.hContactPhone  = _contactPhone . _householdContact $ h
  , Api.hTotalOrders   = householdTotalOrders h
  , Api.hTotalPayments = householdTotalPayments   h
  , Api.hBalance       = householdBalance h
  }

apiHouseholdPayment :: Payment -> Api.HouseholdPayment
apiHouseholdPayment p = Api.HouseholdPayment 
  { Api.hpId = fromPaymentId . _paymentId $ p
  , Api.hpHouseholdId = fromHouseholdId . _paymentHouseholdId $ p
  , Api.hpDate = apiToNearestSecond . _paymentDate $ p
  , Api.hpAmount = _paymentAmount p
  }

apiCollectiveOrder :: [(ProductCode, ProductId)] -> Order -> Api.CollectiveOrder
apiCollectiveOrder productIds o = Api.CollectiveOrder
    { Api.coId                    = fromOrderId . _orderId                                     . _orderInfo $ o
    , Api.coOrderCreatedDate      = apiToNearestSecond . _orderCreated                         . _orderInfo $ o
    , Api.coOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
    , Api.coOrderCreatedByName    = fmap _householdName                      . _orderCreatedBy . _orderInfo $ o
    , Api.coOrderIsPlaced         = (== OrderPlaced) . _orderStatus $ o
    , Api.coOrderIsAbandoned      = (== OrderAbandoned) . _orderStatus $ o
    , Api.coIsComplete            = orderIsComplete o
    , Api.coAllHouseholdsUpToDate = not $ orderIsAwaitingCatalogueUpdateConfirm o
    , Api.coTotalExcVat           = _moneyExcVat $ case orderAdjustment o of
                                                     Just a -> _orderAdjNewTotal a
                                                     _      -> orderTotal $ o
    , Api.coTotalIncVat           = _moneyIncVat $ case orderAdjustment o of
                                                     Just a -> _orderAdjNewTotal a
                                                     _      -> orderTotal $ o
    , Api.coAdjustment            = apiOrderAdjustment (orderTotal o) $ orderAdjustment o
    , Api.coItems = M.elems $ apiOrderItem productIds <$> orderItemsToPlace o
    }

apiPastCollectiveOrder :: [(ProductCode, ProductId)] -> Order -> Api.PastCollectiveOrder
apiPastCollectiveOrder productIds o = Api.PastCollectiveOrder
    { Api.pcoId                    = fromOrderId . _orderId                                     . _orderInfo $ o
    , Api.pcoOrderCreatedDate      = apiToNearestSecond . _orderCreated                         . _orderInfo $ o
    , Api.pcoOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
    , Api.pcoOrderCreatedByName    = fmap _householdName                      . _orderCreatedBy . _orderInfo $ o
    , Api.pcoOrderIsPlaced         = (== OrderPlaced) . _orderStatus $ o
    , Api.pcoOrderIsAbandoned      = (== OrderAbandoned) . _orderStatus $ o
    , Api.pcoIsComplete            = orderIsComplete o
    , Api.pcoIsAbandoned           = (== OrderAbandoned) . _orderStatus $ o
    , Api.pcoIsReconciled          = orderIsReconciled o
    , Api.pcoAllHouseholdsUpToDate = True
    , Api.pcoTotalExcVat           = _moneyExcVat adjustedTotal
    , Api.pcoTotalIncVat           = _moneyIncVat adjustedTotal
    , Api.pcoAdjustment            = apiOrderAdjustment total $ adjustment
    , Api.pcoItems = M.elems $ apiOrderItem productIds <$> items
    }
  where 
    adjustedTotal = case adjustment of
                      Just a -> _orderAdjNewTotal a
                      _      -> total
    total = orderTotal o
    adjustment = orderAdjustment o
    items = orderItemsToPlace o

apiOrderAdjustment :: Money -> Maybe V2.Domain.OrderAdjustment -> Maybe Api.OrderAdjustment
apiOrderAdjustment total (Just _) = Just $ Api.OrderAdjustment
  { Api.oaOldTotalExcVat = _moneyExcVat total
  , Api.oaOldTotalIncVat = _moneyIncVat total
  }
apiOrderAdjustment _ _ = Nothing

apiHouseholdOrder :: [(ProductCode, ProductId)] -> V2.Domain.HouseholdOrder -> Api.HouseholdOrder
apiHouseholdOrder productIds ho = Api.HouseholdOrder
    { Api.hoOrderId            = fromOrderId . _orderId                                     . _householdOrderOrderInfo $ ho
    , Api.hoOrderCreatedDate   = apiToNearestSecond . _orderCreated                         . _householdOrderOrderInfo $ ho
    , Api.hoOrderCreatedBy     = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , Api.hoOrderCreatedByName = fmap _householdName                      . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , Api.hoOrderIsPlaced      = (== OrderPlaced) . _householdOrderOrderStatus $ ho
    , Api.hoOrderIsAbandoned   = (== OrderAbandoned) . _householdOrderOrderStatus $ ho
    , Api.hoHouseholdId        = fromHouseholdId . _householdId . _householdOrderHouseholdInfo $ ho
    , Api.hoHouseholdName      = _householdName                 . _householdOrderHouseholdInfo $ ho
    , Api.hoIsComplete         = (== HouseholdOrderComplete) . _householdOrderStatus $ ho
    , Api.hoIsAbandoned        = (== HouseholdOrderAbandoned) . _householdOrderStatus $ ho
    , Api.hoIsOpen             = apiHouseholdOrderIsOpen ho
    , Api.hoTotalExcVat        = _moneyExcVat $ case householdOrderAdjustment ho of
                                                  Just a -> _orderAdjNewTotal a
                                                  _      -> householdOrderTotal ho
    , Api.hoTotalIncVat        = _moneyIncVat $ case householdOrderAdjustment ho of
                                                  Just a -> _orderAdjNewTotal a
                                                  _      -> householdOrderTotal ho
    , Api.hoAdjustment         = apiOrderAdjustment (householdOrderTotal ho) $ householdOrderAdjustment ho
    , Api.hoItems = M.elems $ apiOrderItem productIds <$> _householdOrderItems ho
    }

apiHouseholdOrderIsOpen :: V2.Domain.HouseholdOrder -> Bool
apiHouseholdOrderIsOpen = (/= HouseholdOrderComplete) . _householdOrderStatus
                     .&&. (/= HouseholdOrderAbandoned) . _householdOrderStatus
                     .&&. (/= OrderAbandoned) . _householdOrderOrderStatus
                     .&&. (/= OrderPlaced) . _householdOrderOrderStatus

apiPastHouseholdOrder :: [(ProductCode, ProductId)] -> V2.Domain.HouseholdOrder -> Api.PastHouseholdOrder
apiPastHouseholdOrder productIds ho = Api.PastHouseholdOrder
    { Api.phoOrderId            = fromOrderId . _orderId                                     . _householdOrderOrderInfo $ ho
    , Api.phoOrderCreatedDate   = apiToNearestSecond . _orderCreated                         . _householdOrderOrderInfo $ ho
    , Api.phoOrderCreatedBy     = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , Api.phoOrderCreatedByName = fmap _householdName                      . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , Api.phoOrderIsPlaced      = (== OrderPlaced) . _householdOrderOrderStatus $ ho
    , Api.phoOrderIsAbandoned   = (== OrderAbandoned) . _householdOrderOrderStatus $ ho
    , Api.phoHouseholdId        = fromHouseholdId . _householdId . _householdOrderHouseholdInfo $ ho
    , Api.phoHouseholdName      = _householdName                 . _householdOrderHouseholdInfo $ ho
    , Api.phoIsComplete         = (== HouseholdOrderComplete) . _householdOrderStatus $ ho
    , Api.phoIsAbandoned        = (== HouseholdOrderAbandoned) . _householdOrderStatus $ ho
    , Api.phoIsOpen             = apiHouseholdOrderIsOpen ho
    , Api.phoIsReconciled       = householdOrderIsReconciled ho
    , Api.phoTotalExcVat        = _moneyExcVat $ case householdOrderAdjustment ho of
                                                   Just a -> _orderAdjNewTotal a
                                                   _      -> householdOrderTotal ho
    , Api.phoTotalIncVat        = _moneyIncVat $ case householdOrderAdjustment ho of
                                                   Just a -> _orderAdjNewTotal a
                                                   _      -> householdOrderTotal ho
    , Api.phoAdjustment         = apiOrderAdjustment (householdOrderTotal ho) $ householdOrderAdjustment ho
    , Api.phoItems = M.elems $ apiOrderItem productIds <$> _householdOrderItems ho
    }

apiOrderItem :: [(ProductCode, ProductId)] -> V2.Domain.OrderItem -> Api.OrderItem
apiOrderItem productIds i = Api.OrderItem
  { Api.oiProductId          = fromMaybe 0 $ fromProductId <$> lookup (itemProductCode i) productIds
  , Api.oiProductCode        = fromProductCode . itemProductCode $ i
  , Api.oiProductName        = _productName . _productInfo . _itemProduct $ i
  , Api.oiProductVatRate     = apiVatRate . _vatRateType . _priceVatRate . itemProductPrice $ i
  , Api.oiProductPriceExcVat = _moneyExcVat . _priceAmount $ case _itemAdjustment i of
                                                               Just a -> _itemAdjNewPrice a
                                                               _      -> itemProductPrice $ i
  , Api.oiProductPriceIncVat = _moneyIncVat . _priceAmount $ case _itemAdjustment i of
                                                               Just a -> _itemAdjNewPrice a
                                                               _      -> itemProductPrice $ i
  , Api.oiItemQuantity       = case _itemAdjustment i of
                                 Just a -> _itemAdjNewQuantity a
                                 _      -> _itemQuantity i
  , Api.oiItemTotalExcVat    = _moneyExcVat $ case _itemAdjustment i of
                                                Just a -> itemAdjNewTotal a
                                                _      -> itemTotal $ i
  , Api.oiItemTotalIncVat    = _moneyIncVat $ case _itemAdjustment i of
                                                Just a -> itemAdjNewTotal a
                                                _      -> itemTotal $ i
  , Api.oiBiodynamic         = _productIsBiodynamic . _productFlags . _itemProduct $ i
  , Api.oiFairTrade          = _productIsFairTrade  . _productFlags . _itemProduct $ i
  , Api.oiGlutenFree         = _productIsGlutenFree . _productFlags . _itemProduct $ i
  , Api.oiOrganic            = _productIsOrganic    . _productFlags . _itemProduct $ i
  , Api.oiAddedSugar         = _productIsAddedSugar . _productFlags . _itemProduct $ i
  , Api.oiVegan              = _productIsVegan      . _productFlags . _itemProduct $ i
  , Api.oiPacked             = _itemIsPacked $ i
  , Api.oiAdjustment         = apiOrderItemAdjustment i $ _itemAdjustment i
  }

apiOrderItemAdjustment :: V2.Domain.OrderItem -> Maybe V2.Domain.OrderItemAdjustment -> Maybe Api.OrderItemAdjustment
apiOrderItemAdjustment i (Just a) = Just $ Api.OrderItemAdjustment
  { Api.oiaOldProductPriceExcVat = _moneyExcVat . _priceAmount  . _productPrice . _productInfo . _itemProduct $ i
  , Api.oiaOldProductPriceIncVat = _moneyIncVat . _priceAmount  . _productPrice . _productInfo . _itemProduct $ i
  , Api.oiaOldItemQuantity       = _itemQuantity i
  , Api.oiaOldItemTotalExcVat    = _moneyExcVat . itemTotal $ i
  , Api.oiaOldItemTotalIncVat    = _moneyIncVat . itemTotal $ i
  , Api.oiaProductDiscontinued   = _itemAdjIsDiscontinued a
  }
apiOrderItemAdjustment _ _ = Nothing

apiProductCatalogueEntry :: V2.Domain.ProductCatalogueEntry -> Api.ProductCatalogueEntry
apiProductCatalogueEntry e = Api.ProductCatalogueEntry
  { Api.pceCode = fromProductCode . _catalogueEntryCode $ e
  , Api.pceName = buildProductName e
  , Api.pcePriceExcVat = _moneyExcVat . _priceAmount . _catalogueEntryPrice $ e
  , Api.pcePriceIncVat = _moneyIncVat . _priceAmount . _catalogueEntryPrice $ e
  , Api.pceVatRate = apiVatRate . _vatRateType . _priceVatRate . _catalogueEntryPrice $ e
  , Api.pceBiodynamic = _catalogueEntryBiodynamic e
  , Api.pceFairTrade = _catalogueEntryFairTrade e
  , Api.pceGlutenFree = _catalogueEntryGlutenFree e
  , Api.pceOrganic = _catalogueEntryOrganic e
  , Api.pceAddedSugar = _catalogueEntryAddedSugar e
  , Api.pceVegan = _catalogueEntryVegan e
  , Api.pceCategory = _catalogueEntryCategory e
  , Api.pceBrand = _catalogueEntryBrand e
  }

apiGroupSettings :: V2.Domain.OrderGroup -> Api.GroupSettings
apiGroupSettings g = Api.GroupSettings
  { Api.gsEnablePayments = _groupSettingsPaymentsEnabled . _groupSettings $ g
  }

apiVatRate :: V2.Domain.VatRateType -> Api.VatRate
apiVatRate V2.Domain.Zero = Api.Zero
apiVatRate V2.Domain.Standard = Api.Standard
apiVatRate V2.Domain.Reduced = Api.Reduced

apiToNearestSecond :: UTCTime -> UTCTime
apiToNearestSecond (UTCTime day time) = UTCTime day (fromIntegral (floor (realToFrac time :: Double) :: Int))