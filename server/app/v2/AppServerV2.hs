module AppServerV2 (appServerV2) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString as B (ByteString)
import           Data.Text (Text)
import           Data.Text as T (unpack)
import           Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import           Servant
import           Servant.Multipart

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
    households = withRepository config $ \repo -> do
      households <- liftIO $ getHouseholds repo
      return $ apiHousehold <$> households

    collectiveOrder :: Handler (Maybe Api.CollectiveOrder)
    collectiveOrder = withRepository config $ \repo -> do
      order <- liftIO $ getOrder repo
      return $ apiOrder <$> order

    pastCollectiveOrders :: Handler [Api.CollectiveOrder]
    pastCollectiveOrders = withRepository config $ \repo -> do
      orders <- liftIO $ getPastOrders repo
      return $ apiOrder <$> orders

commandServerV2 :: RepositoryConfig -> Server CommandApiV2
commandServerV2 config  = 
         createOrderForHousehold
    :<|> createOrder
    :<|> abandonHouseholdOrder
    :<|> completeHouseholdOrder
    :<|> reopenHouseholdOrder
    :<|> ensureHouseholdOrderItem
    :<|> removeHouseholdOrderItem
  where
    createOrderForHousehold :: Int -> Handler Int
    createOrderForHousehold householdId = withRepository config $ \repo -> do
      date <- liftIO $ getCurrentTime
      orderId <- liftIO $ newOrder repo $ OrderSpec date $ Just $ HouseholdId householdId
      return $ fromOrderId orderId

    createOrder :: Handler Int
    createOrder = withRepository config $ \repo -> do
      date <- liftIO getCurrentTime
      orderId <- liftIO $ newOrder repo $ OrderSpec date Nothing
      return $ fromOrderId orderId

    abandonHouseholdOrder :: Int -> Int -> Handler ()
    abandonHouseholdOrder orderId householdId = withRepository config $ \repo -> do
      order <- MaybeT $ getHouseholdOrder repo (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.abandonHouseholdOrder order
      liftIO $ updateHouseholdOrder repo order'
      return ()

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder orderId householdId = withRepository config $ \repo -> do
      order <- MaybeT $ getHouseholdOrder repo (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.completeHouseholdOrder order
      liftIO $ updateHouseholdOrder repo order'
      return ()

    reopenHouseholdOrder :: Int -> Int -> Handler ()
    reopenHouseholdOrder orderId householdId = withRepository config $ \repo -> do
      order <- MaybeT $ getHouseholdOrder repo (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.reopenHouseholdOrder order
      liftIO $ updateHouseholdOrder repo order'
      return ()

    ensureHouseholdOrderItem :: Int -> Int -> String -> Api.HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem orderId householdId productCode details = withRepository config $ \repo -> do
      date <- liftIO getCurrentTime
      order <- MaybeT $ createHouseholdOrder repo (OrderId orderId) (HouseholdId householdId) date
      product <- MaybeT $ createProduct repo productCode
      let order' = updateHouseholdOrderItem product (hoidetQuantity details) order
      liftIO $ updateHouseholdOrder repo order'
      return ()

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem orderId householdId productId = withRepository config $ \repo -> do
      order <- MaybeT $ getHouseholdOrder repo (OrderId orderId) (HouseholdId householdId)
      let order' = DomainV2.removeHouseholdOrderItem (ProductId productId) order
      liftIO $ updateHouseholdOrder repo order'
      return ()

withRepository :: RepositoryConfig -> (Repository -> MaybeT IO a) -> Handler a
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
  , hTotalOrders   = _householdTotalOrders h
  , hTotalPayments = _householdTotalPayments h
  , hBalance       = _householdBalance h
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
  , coAllHouseholdsUpToDate = orderIsAllHouseholdsUpToDate o
  , coTotalExcVat           = _moneyExcVat $ case _orderAdjustment o of
                                               Just a -> _orderAdjNewTotal a
                                               _      -> _orderTotal $ o
  , coTotalIncVat           = _moneyIncVat $ case _orderAdjustment o of
                                               Just a -> _orderAdjNewTotal a
                                               _      -> _orderTotal $ o
  , coAdjustment            = apiOrderAdjustment o $ _orderAdjustment o
  , coItems = apiOrderItem <$> _orderItems o
  }

apiOrderAdjustment :: DomainV2.Order -> Maybe DomainV2.OrderAdjustment -> Maybe Api.OrderAdjustment
apiOrderAdjustment o (Just _) = Just $ Api.OrderAdjustment
  { oaOldTotalExcVat = _moneyExcVat . _orderTotal $ o
  , oaOldTotalIncVat = _moneyIncVat . _orderTotal $ o
  }
apiOrderAdjustment _ _ = Nothing

apiOrderItem :: DomainV2.OrderItem -> Api.OrderItem
apiOrderItem i = Api.OrderItem
  { oiProductId          = fromProductId . _productId                   . _productInfo . _itemProduct $ i 
  , oiProductCode        = _productCode                                 . _productInfo . _itemProduct $ i
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
                                            Just a -> _itemAdjNewTotal a
                                            _      -> _itemTotal $ i
  , oiItemTotalIncVat    = _moneyIncVat $ case _itemAdjustment i of
                                            Just a -> _itemAdjNewTotal a
                                            _      -> _itemTotal $ i
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
  , oiaOldItemTotalExcVat    = _moneyExcVat . _itemTotal $ i
  , oiaOldItemTotalIncVat    = _moneyIncVat . _itemTotal $ i
  , oiaProductDiscontinued   = _itemAdjIsDiscontinued a
  }
apiOrderItemAdjustment _ _ = Nothing