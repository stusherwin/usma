{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AppServerV2 where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack, toStrict)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime)
import           Data.UUID (toString)
import           Data.UUID.V1 (nextUUID)
import           Safe (headMay)
import           Servant
import           Servant.Multipart (MultipartData(..), FileData(..), Mem)

import           AppApiV2 as Api
import           Types as Api
import           Config (Config(..))
import           CsvExport (exportOrderItems, exportOrderItemsByHousehold)
import           DomainV2
import           RepositoryV2 as R
import           SumaCatalogue (fetchProductImage)
import           ReconcileSumaOrderFile (parseOrderFileDetails, parseOrderFileUpdates)

appServerV2 :: Config -> Text -> Server Api.AppApiV2
appServerV2 config groupKey =
         queryServerV2 repoConfig
    :<|> commandServerV2 repoConfig
  where
    repoConfig = RepositoryConfig (connectionStringV2 config) (T.unpack groupKey)

queryServerV2 :: RepositoryConfig -> Server Api.QueryApiV2
queryServerV2 config = 
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

      return $ ApiData collectiveOrder' 
                       pastCollectiveOrders'
                       householdOrders' 
                       pastHouseholdOrders' 
                       households' 
                       householdPayments' 
                       groupSettings'

    productCatalogueData :: Handler Api.ProductCatalogueApiData
    productCatalogueData = withRepository config $ \(repo, _) -> do
      productCatalogue <- map apiProductCatalogueEntry . getEntries <$> (liftIO $ getProductCatalogue repo)
      categories <- liftIO $ getProductCatalogueCategories repo
      brands <- liftIO $ getProductCatalogueBrands repo
    
      return $ ProductCatalogueApiData productCatalogue categories brands

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

    productCatalogue :: Handler [Api.ProductCatalogueEntry]
    productCatalogue = withRepository config $ \(repo, _) -> do
      catalogue <- liftIO $ getProductCatalogue repo
      let entries = getEntries catalogue
      return $ apiProductCatalogueEntry <$> entries
    
    productImage :: String -> Handler BL.ByteString
    productImage code = withRepository config $ \(repo, _) -> do
      MaybeT $ liftIO $ fetchProductImage repo code

    collectiveOrderDownload :: Handler FileDownload
    collectiveOrderDownload = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getCurrentOrder repo groupId
        return $ fileDownload "order.csv" $ exportOrderItems order

    householdOrdersDownload :: Handler FileDownload
    householdOrdersDownload = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getCurrentOrder repo groupId
        return $ fileDownload "order.csv" $ exportOrderItemsByHousehold order

    pastCollectiveOrderDownload :: Int -> Handler FileDownload
    pastCollectiveOrderDownload orderId = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getOrder repo groupId (OrderId orderId)
        return $ fileDownload "order.csv" $ exportOrderItems order

    pastHouseholdOrdersDownload :: Int -> Handler FileDownload
    pastHouseholdOrdersDownload orderId = withRepository config $ \(repo, groupId) -> do
        order <- MaybeT $ liftIO $ getOrder repo groupId (OrderId orderId)
        return $ fileDownload "order.csv" $ exportOrderItemsByHousehold order

    productCatalogueCategories :: Handler [String]
    productCatalogueCategories = withRepository config $ \(repo, _) -> do
      liftIO $ getProductCatalogueCategories repo

    productCatalogueBrands :: Handler [String]
    productCatalogueBrands = withRepository config $ \(repo, _) -> do
      liftIO $ getProductCatalogueBrands repo

    groupSettings :: Handler Api.GroupSettings
    groupSettings = withRepository config $ \(repo, groupId) -> do
      group <- liftIO $ getOrderGroup repo groupId
      return $ apiGroupSettings group

commandServerV2 :: RepositoryConfig -> Server CommandApiV2
commandServerV2 config  = 
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
      let order' = DomainV2.abandonOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    placeOrder :: Int -> Handler ()
    placeOrder orderId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = DomainV2.placeOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    abandonHouseholdOrder :: Int -> Int -> Handler ()
    abandonHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = DomainV2.abandonHouseholdOrder (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = DomainV2.completeHouseholdOrder (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    reopenHouseholdOrder :: Int -> Int -> Handler ()  
    reopenHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      catalogue <- liftIO $ getProductCatalogueForOrder repo groupId (OrderId orderId)
      let order' = DomainV2.reopenHouseholdOrder catalogue (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    ensureHouseholdOrderItem :: Int -> Int -> String -> Api.HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem orderId householdId productCode details = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      household <- MaybeT $ getHouseholdInfo repo groupId (HouseholdId householdId)
      catalogue <- liftIO $ getProductCatalogueForCode repo (ProductCode productCode)
      let order' = addOrUpdateHouseholdOrderItems catalogue household [(ProductCode productCode, hoidetQuantity details)] order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    ensureAllItemsFromPastHouseholdOrder :: Int -> Int -> Int -> Handler ()
    ensureAllItemsFromPastHouseholdOrder orderId householdId pastOrderId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      pastOrder <- MaybeT $ getOrder repo groupId (OrderId pastOrderId)
      household <- MaybeT $ getHouseholdInfo repo groupId (HouseholdId householdId)
      catalogue <- liftIO $ getProductCatalogueForOrder repo groupId (OrderId pastOrderId)
      let order' = addItemsFromPastOrder catalogue household pastOrder order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem orderId householdId productId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      productCode <- MaybeT $ getProductCode repo (ProductId productId)
      let order' = DomainV2.removeHouseholdOrderItem (HouseholdId householdId) productCode order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    createHousehold :: HouseholdDetails -> Handler Int
    createHousehold details = withRepository config $ \(repo, groupId) -> do
        householdId <- liftIO $ R.createHousehold repo groupId spec
        return $ fromHouseholdId householdId
      where
        contact = Contact (hdetContactName details)
                          (hdetContactEmail details)
                          (hdetContactPhone details)
        spec = HouseholdSpec (hdetName details) contact

    updateHousehold :: Int -> HouseholdDetails -> Handler ()
    updateHousehold householdId details = withRepository config $ \(repo, groupId) -> do
        household <- MaybeT $ getHousehold repo groupId (HouseholdId householdId)
        let household' = DomainV2.updateHousehold name contact household
        liftIO $ setHousehold repo groupId (household, household')
      where
        name = hdetName details
        contact = Contact (hdetContactName details)
                          (hdetContactEmail details)
                          (hdetContactPhone details)

    archiveHousehold :: Int -> Handler ()
    archiveHousehold householdId = withRepository config $ \(repo, groupId) ->
      liftIO $ removeHousehold repo groupId (HouseholdId householdId)

    createHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler Int
    createHouseholdPayment householdId details = withRepository config $ \(repo, groupId) -> do
        paymentId <- liftIO $ createPayment repo groupId $ PaymentSpec (HouseholdId householdId) date amount
        return $ fromPaymentId paymentId
      where
        date = UTCTime (hpdetDate details) (secondsToDiffTime 0)
        amount = hpdetAmount details

    updateHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler ()
    updateHouseholdPayment paymentId details = withRepository config $ \(repo, groupId) -> do
        payment <- MaybeT $ getPayment repo groupId (PaymentId paymentId)
        let payment' = updatePayment date amount payment
        liftIO $ setPayment repo groupId (payment, payment')
      where
        date = UTCTime (hpdetDate details) (secondsToDiffTime 0)
        amount = hpdetAmount details
    
    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment paymentId = withRepository config $ \(repo, groupId) ->
      liftIO $ removePayment repo groupId (PaymentId paymentId)

    uploadProductCatalogue :: MultipartData Mem -> Handler ()
    uploadProductCatalogue multipartData = withRepository config $ \(repo, _) -> do
      date <- liftIO getCurrentTime
      fileContents <- uploadSingleFile multipartData
      vatRates <- liftIO $ getVatRates repo
      orders <- liftIO $ getCurrentOrders repo Nothing
      let catalogue = parseCatalogue vatRates date $ BL.unpack fileContents
      let orders' = map (applyCatalogueUpdate catalogue) orders
      liftIO $ setProductCatalogue repo catalogue
      liftIO $ setOrders repo (orders, orders')
      return ()

    acceptCatalogueUpdates :: Int -> Int -> Handler ()
    acceptCatalogueUpdates orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      let order' = DomainV2.acceptCatalogueUpdate (HouseholdId householdId) order
      liftIO $ setOrders repo ([order], [order'])
      return ()

    reconcileOrderItem :: Int -> Int -> ReconcileOrderItemDetails -> Handler ()
    reconcileOrderItem orderId productId details = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      productCode <- MaybeT $ getProductCode repo (ProductId productId)
      let updates = map (\h -> (HouseholdId $ hqdetHouseholdId h, OrderItemSpec productCode (roidetProductPriceExcVat details) (hqdetItemQuantity h)))
                  $ roidetHouseholdQuantities details
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

uploadSingleFile :: MultipartData Mem -> MaybeT IO BL.ByteString
uploadSingleFile multipartData = do
  file <- MaybeT $ return $ headMay $ files multipartData
  return $ fdPayload file

fileDownload :: String -> BL.ByteString -> FileDownload
fileDownload fileName file = addHeader header file
  where
    header = T.pack $ "attachment; filename=\"" ++ fileName ++ "\""

withRepository :: RepositoryConfig -> ((Repository, OrderGroupId) -> MaybeT IO a) -> Handler a
withRepository config query = do
  result <- liftIO $ runMaybeT $ connect config $ query
  case result of
    Just r -> return r
    _ -> throwError err404

apiHousehold :: DomainV2.Household -> Api.Household 
apiHousehold h = Api.Household
  { hId            = fromHouseholdId . _householdId . _householdInfo $ h
  , hName          = _householdName                 . _householdInfo $ h
  , hContactName   = _contactName  . _householdContact $ h
  , hContactEmail  = _contactEmail . _householdContact $ h
  , hContactPhone  = _contactPhone . _householdContact $ h
  , hTotalOrders   = householdTotalOrders h
  , hTotalPayments = householdTotalPayments h
  , hBalance       = householdBalance h
  }

apiHouseholdPayment :: Payment -> HouseholdPayment
apiHouseholdPayment p = HouseholdPayment 
  { hpId = fromPaymentId . _paymentId $ p
  , hpHouseholdId = fromHouseholdId . _paymentHouseholdId $ p
  , hpDate = apiToNearestSecond . _paymentDate $ p
  , hpAmount = _paymentAmount p
  }

apiCollectiveOrder :: [(ProductCode, ProductId)] -> Order -> Api.CollectiveOrder
apiCollectiveOrder productIds o = Api.CollectiveOrder
    { coId                    = fromOrderId . _orderId                                     . _orderInfo $ o
    , coOrderCreatedDate      = apiToNearestSecond . _orderCreated                         . _orderInfo $ o
    , coOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
    , coOrderCreatedByName    = fmap _householdName                      . _orderCreatedBy . _orderInfo $ o
    , coOrderIsPlaced         = apiOrderIsPlaced o
    , coOrderIsAbandoned      = apiOrderIsAbandoned o
    , coIsComplete            = orderIsComplete o
    , coAllHouseholdsUpToDate = not $ orderIsAwaitingCatalogueUpdateConfirm o
    , coTotalExcVat           = _moneyExcVat $ case orderAdjustment o of
                                                 Just a -> _orderAdjNewTotal a
                                                 _      -> orderTotal $ o
    , coTotalIncVat           = _moneyIncVat $ case orderAdjustment o of
                                                 Just a -> _orderAdjNewTotal a
                                                 _      -> orderTotal $ o
    , coAdjustment            = apiOrderAdjustment (orderTotal o) $ orderAdjustment o
    , coItems = apiOrderItem productIds <$> orderItems o
    }

apiPastCollectiveOrder :: [(ProductCode, ProductId)] -> Order -> Api.PastCollectiveOrder
apiPastCollectiveOrder productIds o = Api.PastCollectiveOrder
    { pcoId                    = fromOrderId . _orderId                                     . _orderInfo $ o
    , pcoOrderCreatedDate      = apiToNearestSecond . _orderCreated                         . _orderInfo $ o
    , pcoOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
    , pcoOrderCreatedByName    = fmap _householdName                      . _orderCreatedBy . _orderInfo $ o
    , pcoOrderIsPlaced         = apiOrderIsPlaced o
    , pcoOrderIsAbandoned      = apiOrderIsAbandoned o
    , pcoIsComplete            = orderIsComplete o
    , pcoIsAbandoned           = apiOrderIsAbandoned o
    , pcoIsReconciled          = orderIsReconciled o
    , pcoAllHouseholdsUpToDate = True
    , pcoTotalExcVat           = _moneyExcVat adjustedTotal
    , pcoTotalIncVat           = _moneyIncVat adjustedTotal
    , pcoAdjustment            = apiOrderAdjustment total $ adjustment
    , pcoItems = apiOrderItem productIds <$> items
    }
  where 
    adjustedTotal = case adjustment of
                      Just a -> _orderAdjNewTotal a
                      _      -> total
    total = orderTotal o
    adjustment = orderAdjustment o
    items = orderItems o

apiOrderAdjustment :: Money -> Maybe DomainV2.OrderAdjustment -> Maybe Api.OrderAdjustment
apiOrderAdjustment total (Just _) = Just $ Api.OrderAdjustment
  { oaOldTotalExcVat = _moneyExcVat total
  , oaOldTotalIncVat = _moneyIncVat total
  }
apiOrderAdjustment _ _ = Nothing

apiHouseholdOrder :: [(ProductCode, ProductId)] -> DomainV2.HouseholdOrder -> Api.HouseholdOrder
apiHouseholdOrder productIds ho = Api.HouseholdOrder
    { hoOrderId            = fromOrderId . _orderId                                     . _householdOrderOrderInfo $ ho
    , hoOrderCreatedDate   = apiToNearestSecond . _orderCreated                         . _householdOrderOrderInfo $ ho
    , hoOrderCreatedBy     = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , hoOrderCreatedByName = fmap _householdName                      . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , hoOrderIsPlaced      = apiHouseholdOrderOrderIsPlaced ho
    , hoOrderIsAbandoned   = apiHouseholdOrderOrderIsAbandoned ho
    , hoHouseholdId        = fromHouseholdId . _householdId . _householdOrderHouseholdInfo $ ho
    , hoHouseholdName      = _householdName                 . _householdOrderHouseholdInfo $ ho
    , hoIsComplete         = apiHouseholdOrderIsComplete ho
    , hoIsAbandoned        = apiHouseholdOrderIsAbandoned ho
    , hoIsOpen             = apiHouseholdOrderIsOpen ho
    , hoTotalExcVat        = _moneyExcVat $ case householdOrderAdjustment ho of
                                              Just a -> _orderAdjNewTotal a
                                              _      -> householdOrderTotal $ ho
    , hoTotalIncVat        = _moneyIncVat $ case householdOrderAdjustment ho of
                                              Just a -> _orderAdjNewTotal a
                                              _      -> householdOrderTotal $ ho
    , hoAdjustment         = apiOrderAdjustment (householdOrderTotal ho) $ householdOrderAdjustment ho
    , hoItems = apiOrderItem productIds <$> _householdOrderItems ho
    }

apiOrderIsAbandoned :: DomainV2.Order -> Bool
apiOrderIsAbandoned = (== OrderAbandoned) . _orderStatus

apiOrderIsPlaced :: DomainV2.Order -> Bool
apiOrderIsPlaced = (== OrderPlaced) . _orderStatus

apiHouseholdOrderOrderIsAbandoned :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderOrderIsAbandoned = (== OrderAbandoned) . _householdOrderOrderStatus

apiHouseholdOrderOrderIsPlaced :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderOrderIsPlaced = (== OrderPlaced) . _householdOrderOrderStatus

apiHouseholdOrderIsAbandoned :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderIsAbandoned = (== HouseholdOrderAbandoned) . _householdOrderStatus

apiHouseholdOrderIsComplete :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderIsComplete = (== HouseholdOrderComplete) . _householdOrderStatus

apiHouseholdOrderIsOpen :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderIsOpen ho = (not (apiHouseholdOrderIsComplete ho) && not (apiHouseholdOrderIsAbandoned ho) && not (apiHouseholdOrderOrderIsAbandoned ho) && not (apiHouseholdOrderOrderIsPlaced ho))

apiPastHouseholdOrder :: [(ProductCode, ProductId)] -> DomainV2.HouseholdOrder -> Api.PastHouseholdOrder
apiPastHouseholdOrder productIds ho = Api.PastHouseholdOrder
    { phoOrderId            = fromOrderId . _orderId                                     . _householdOrderOrderInfo $ ho
    , phoOrderCreatedDate   = apiToNearestSecond . _orderCreated                         . _householdOrderOrderInfo $ ho
    , phoOrderCreatedBy     = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , phoOrderCreatedByName = fmap _householdName                      . _orderCreatedBy . _householdOrderOrderInfo $ ho
    , phoOrderIsPlaced      = apiHouseholdOrderOrderIsPlaced ho
    , phoOrderIsAbandoned   = apiHouseholdOrderOrderIsAbandoned ho
    , phoHouseholdId        = fromHouseholdId . _householdId . _householdOrderHouseholdInfo $ ho
    , phoHouseholdName      = _householdName                 . _householdOrderHouseholdInfo $ ho
    , phoIsComplete         = apiHouseholdOrderIsComplete ho
    , phoIsAbandoned        = apiHouseholdOrderIsAbandoned ho
    , phoIsOpen             = apiHouseholdOrderIsOpen ho
    , phoIsReconciled       = apiHouseholdOrderIsReconciled ho
    , phoTotalExcVat        = _moneyExcVat $ case householdOrderAdjustment ho of
                                              Just a -> _orderAdjNewTotal a
                                              _      -> householdOrderTotal $ ho
    , phoTotalIncVat        = _moneyIncVat $ case householdOrderAdjustment ho of
                                              Just a -> _orderAdjNewTotal a
                                              _      -> householdOrderTotal $ ho
    , phoAdjustment         = apiOrderAdjustment (householdOrderTotal ho) $ householdOrderAdjustment ho
    , phoItems = apiOrderItem productIds <$> _householdOrderItems ho
    }

apiHouseholdOrderIsReconciled :: DomainV2.HouseholdOrder -> Bool
apiHouseholdOrderIsReconciled ho = apiHouseholdOrderOrderIsPlaced ho && (all (isJust . _itemAdjustment) . _householdOrderItems $ ho)

apiOrderItem :: [(ProductCode, ProductId)] -> DomainV2.OrderItem -> Api.OrderItem
apiOrderItem productIds i = Api.OrderItem
  { oiProductId          = fromMaybe 0 $ fromProductId <$> lookup (itemProductCode i) productIds
  , oiProductCode        = fromProductCode . itemProductCode $ i
  , oiProductName        = _productName . _productInfo . _itemProduct $ i
  , oiProductVatRate     = apiVatRate . _vatRateType . _priceVatRate . itemProductPrice $ i
  , oiProductPriceExcVat = _moneyExcVat . _priceAmount $ case _itemAdjustment i of
                                                           Just a -> _itemAdjNewPrice a
                                                           _      -> itemProductPrice $ i
  , oiProductPriceIncVat = _moneyIncVat . _priceAmount $ case _itemAdjustment i of
                                                           Just a -> _itemAdjNewPrice a
                                                           _      -> itemProductPrice $ i
  , oiItemQuantity       = case _itemAdjustment i of
                             Just a -> _itemAdjNewQuantity a
                             _      -> _itemQuantity i
  , oiItemTotalExcVat    = _moneyExcVat $ case _itemAdjustment i of
                                            Just a -> itemAdjNewTotal a
                                            _      -> itemTotal $ i
  , oiItemTotalIncVat    = _moneyIncVat $ case _itemAdjustment i of
                                            Just a -> itemAdjNewTotal a
                                            _      -> itemTotal $ i
  , oiBiodynamic         = _productIsBiodynamic . _productFlags . _itemProduct $ i
  , oiFairTrade          = _productIsFairTrade  . _productFlags . _itemProduct $ i
  , oiGlutenFree         = _productIsGlutenFree . _productFlags . _itemProduct $ i
  , oiOrganic            = _productIsOrganic    . _productFlags . _itemProduct $ i
  , oiAddedSugar         = _productIsAddedSugar . _productFlags . _itemProduct $ i
  , oiVegan              = _productIsVegan      . _productFlags . _itemProduct $ i
  , oiAdjustment         = apiOrderItemAdjustment i $ _itemAdjustment i
  }

apiOrderItemAdjustment :: DomainV2.OrderItem -> Maybe DomainV2.OrderItemAdjustment -> Maybe Api.OrderItemAdjustment
apiOrderItemAdjustment i (Just a) = Just $ Api.OrderItemAdjustment
  { oiaOldProductPriceExcVat = _moneyExcVat . _priceAmount  . _productPrice . _productInfo . _itemProduct $ i
  , oiaOldProductPriceIncVat = _moneyIncVat . _priceAmount  . _productPrice . _productInfo . _itemProduct $ i
  , oiaOldItemQuantity       = _itemQuantity i
  , oiaOldItemTotalExcVat    = _moneyExcVat . itemTotal $ i
  , oiaOldItemTotalIncVat    = _moneyIncVat . itemTotal $ i
  , oiaProductDiscontinued   = _itemAdjIsDiscontinued a
  }
apiOrderItemAdjustment _ _ = Nothing

apiProductCatalogueEntry :: DomainV2.ProductCatalogueEntry -> Api.ProductCatalogueEntry
apiProductCatalogueEntry e = Api.ProductCatalogueEntry
  { pceCode = fromProductCode . _catalogueEntryCode $ e
  , pceName = productName e
  , pcePriceExcVat = _moneyExcVat . _priceAmount . _catalogueEntryPrice $ e
  , pcePriceIncVat = _moneyIncVat . _priceAmount . _catalogueEntryPrice $ e
  , pceVatRate = apiVatRate . _vatRateType . _priceVatRate . _catalogueEntryPrice $ e
  , pceBiodynamic = _catalogueEntryBiodynamic e
  , pceFairTrade = _catalogueEntryFairTrade e
  , pceGlutenFree = _catalogueEntryGlutenFree e
  , pceOrganic = _catalogueEntryOrganic e
  , pceAddedSugar = _catalogueEntryAddedSugar e
  , pceVegan = _catalogueEntryVegan e
  , pceCategory = _catalogueEntryCategory e
  , pceBrand = _catalogueEntryBrand e
  }

apiGroupSettings :: DomainV2.OrderGroup -> Api.GroupSettings
apiGroupSettings g = Api.GroupSettings
  { gsEnablePayments = _groupSettingsPaymentsEnabled . _groupSettings $ g
  }

apiVatRate :: DomainV2.VatRateType -> Api.VatRate
apiVatRate DomainV2.Zero = Api.Zero
apiVatRate DomainV2.Standard = Api.Standard
apiVatRate DomainV2.Reduced = Api.Reduced

apiToNearestSecond :: UTCTime -> UTCTime
apiToNearestSecond (UTCTime day time) = UTCTime day (fromIntegral (floor (realToFrac time :: Double) :: Int))