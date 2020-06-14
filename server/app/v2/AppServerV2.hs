module AppServerV2 (appServerV2) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString as B (ByteString)
import           Data.Text (Text)
import           Data.Text as T (unpack)
import           Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Safe (headMay)
import           Servant
import           Servant.Multipart (MultipartData(..), FileData(..))
import           System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)

import           AppApiV2 as Api
import           Config
import           DomainV2
import           RepositoryV2 as R

appServerV2 :: Config -> Text -> Server Api.AppApiV2
appServerV2 config groupKey =
         queryServerV2 repoConfig
    :<|> commandServerV2 repoConfig
  where
    repoConfig = RepositoryConfig (connectionString config) (T.unpack groupKey)

queryServerV2 :: RepositoryConfig -> Server Api.QueryApiV2
queryServerV2 config = 
         households
    :<|> collectiveOrder
    :<|> pastCollectiveOrders
  where
    households :: Handler [Api.Household]
    households = withRepository config $ \(repo, groupId) -> do
      households <- liftIO $ getHouseholds repo (Just groupId)
      return $ apiHousehold <$> households

    collectiveOrder :: Handler (Maybe Api.CollectiveOrder)
    collectiveOrder = withRepository config $ \(repo, groupId) -> do
      order <- liftIO $ getCurrentOrder repo groupId
      return $ apiOrder <$> order

    pastCollectiveOrders :: Handler [Api.CollectiveOrder]
    pastCollectiveOrders = withRepository config $ \(repo, groupId) -> do
      orders <- liftIO $ getPastOrders repo (Just groupId)
      return $ apiOrder <$> orders

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
    :<|> removeHouseholdOrderItem
    :<|> uploadProductCatalogue
    :<|> acceptCatalogueUpdates
    :<|> reconcileOrderItem
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
      order <- MaybeT $ getHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.abandonHouseholdOrder order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.completeHouseholdOrder order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    reopenHouseholdOrder :: Int -> Int -> Handler ()
    reopenHouseholdOrder orderId householdId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.reopenHouseholdOrder order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    ensureHouseholdOrderItem :: Int -> Int -> String -> Api.HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem orderId householdId productCode details = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ createHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId) date
      catalogueEntry <- MaybeT $ getCatalogueEntry repo productCode
      let order' = updateHouseholdOrderItem catalogueEntry (hoidetQuantity details) order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem orderId householdId productId = withRepository config $ \(repo, groupId) -> do
      order <- MaybeT $ getHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      (_, productCode) <- MaybeT $ getProduct repo (ProductId productId)
      let order' = DomainV2.removeHouseholdOrderItem productCode order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    uploadProductCatalogue :: MultipartData -> Handler ()
    uploadProductCatalogue multipartData = withRepository config $ \(repo, _) -> do
      date <- liftIO getCurrentTime
      filePath <- uploadSingleFile multipartData
      file <- liftIO $ readFile filePath
      vatRates <- liftIO $ getVatRates repo
      let catalogue = parseCatalogue vatRates date filePath
      orders <- liftIO $ getCurrentHouseholdOrders repo Nothing
      let orders' = map (applyCatalogueUpdate date catalogue) orders
      liftIO $ setProductCatalogue repo date catalogue
      liftIO $ setHouseholdOrders repo (orders, orders')
      return ()

    acceptCatalogueUpdates :: Int -> Int -> Handler ()
    acceptCatalogueUpdates orderId householdId = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ getHouseholdOrder repo groupId (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.acceptCatalogueUpdates date order
      liftIO $ setHouseholdOrders repo ([order], [order'])
      return ()

    reconcileOrderItem :: Int -> Int -> ReconcileOrderItemDetails -> Handler ()
    reconcileOrderItem orderId productId details = withRepository config $ \(repo, groupId) -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ getOrder repo groupId (OrderId orderId)
      -- Needed to convert ProductId to ProductCode
      -- TODO: Remove ProductId altogether
      (_, productCode) <- MaybeT $ getProduct repo (ProductId productId)
      let updates = map (\h -> (HouseholdId $ hqdetHouseholdId h, (productCode, (roidetProductPriceExcVat details, hqdetItemQuantity h))))
                    $ roidetHouseholdQuantities details
      let order' = reconcileOrderItems date updates order
      liftIO $ setOrders repo ([order], [order'])
      return ()

uploadSingleFile :: MultipartData -> MaybeT IO FilePath
uploadSingleFile multipartData = do
  file <- MaybeT $ return $ headMay $ files multipartData
  liftIO $ createDirectoryIfMissing True "server/data/uploads/"
  day <- liftIO $ utctDay <$> getCurrentTime
  let destFilePath = "server/data/uploads/" ++ (formatTime defaultTimeLocale "%F" day) ++ "-" ++ (T.unpack $ fdFileName file)
  liftIO $ copyFile (fdFilePath file) destFilePath
  return destFilePath

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
  , hContactName   = _householdContactName h
  , hContactEmail  = _householdContactEmail h
  , hContactPhone  = _householdContactPhone h
  , hTotalOrders   = householdTotalOrders h
  , hTotalPayments = householdTotalPayments h
  , hBalance       = householdBalance h
  }

apiOrder :: Order -> Api.CollectiveOrder
apiOrder o = Api.CollectiveOrder
  { coId                    = fromOrderId . _orderId                                     . _orderInfo $ o
  , coOrderCreatedDate      = _orderCreated                                              . _orderInfo $ o
  , coOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
  , coOrderCreatedByName    = fmap _householdName                      . _orderCreatedBy . _orderInfo $ o
  , coOrderIsPlaced         = orderIsPlaced o
  , coOrderIsAbandoned      = orderIsAbandoned o
  , coIsComplete            = orderIsComplete o
  , coAllHouseholdsUpToDate = not $ orderIsAwaitingCatalogueUpdateConfirm o
  , coTotalExcVat           = _moneyExcVat $ case orderAdjustment o of
                                               Just a -> _orderAdjNewTotal a
                                               _      -> orderTotal $ o
  , coTotalIncVat           = _moneyIncVat $ case orderAdjustment o of
                                               Just a -> _orderAdjNewTotal a
                                               _      -> orderTotal $ o
  , coAdjustment            = apiOrderAdjustment o $ orderAdjustment o
  , coItems = apiOrderItem <$> orderItems o
  }

apiOrderAdjustment :: DomainV2.Order -> Maybe DomainV2.OrderAdjustment -> Maybe Api.OrderAdjustment
apiOrderAdjustment o (Just _) = Just $ Api.OrderAdjustment
  { oaOldTotalExcVat = _moneyExcVat . orderTotal $ o
  , oaOldTotalIncVat = _moneyIncVat . orderTotal $ o
  }
apiOrderAdjustment _ _ = Nothing

apiOrderItem :: DomainV2.OrderItem -> Api.OrderItem
apiOrderItem i = Api.OrderItem
  { oiProductId          = fromProductId   . _productId                 . _productInfo . _itemProduct $ i 
  , oiProductCode        = fromProductCode . _productCode               . _productInfo . _itemProduct $ i
  , oiProductName        = _productName                                 . _productInfo . _itemProduct $ i
  , oiProductVatRate     = _vatRateType . _priceVatRate . _productPrice . _productInfo . _itemProduct $ i
  , oiProductPriceExcVat = _moneyExcVat . _priceAmount $ case _itemAdjustment i of
                                                           Just a -> _itemAdjNewPrice a
                                                           _      -> _productPrice . _productInfo . _itemProduct $ i
  , oiProductPriceIncVat = _moneyIncVat . _priceAmount $ case _itemAdjustment i of
                                                           Just a -> _itemAdjNewPrice a
                                                           _      -> _productPrice . _productInfo . _itemProduct $ i
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