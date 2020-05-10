module AppServerV2 (appServerV2) where

import           AppApiV2 as Api
import           Config
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B (ByteString)
import           Data.Text (Text)
import           Data.Text as T (unpack)
import           Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import           Servant
import           Servant.Multipart

import           DomainV2
import           RepositoryV2 as R

appServerV2 :: Config -> Text -> Server Api.AppApiV2
appServerV2 config groupKey =
       queryServerV2 config groupKey
  :<|> commandServerV2 config groupKey
  where
  conn = connectionString config

queryServerV2 :: Config -> Text -> Server Api.QueryApiV2
queryServerV2 config groupKey = 
       households
  :<|> collectiveOrder
  :<|> pastCollectiveOrders
  where
  households :: Handler [Api.Household]
  households = withGroup $ \groupId -> do
    households <- liftIO $ getHouseholds config groupId
    return $ apiHousehold <$> households

  collectiveOrder :: Handler (Maybe Api.CollectiveOrder)
  collectiveOrder = withGroup $ \groupId -> do
    order <- liftIO $ getOrder config groupId
    return $ apiOrder <$> order

  pastCollectiveOrders :: Handler [Api.CollectiveOrder]
  pastCollectiveOrders = withGroup $ \groupId -> do
    orders <- liftIO $ getPastOrders config groupId
    return $ apiOrder <$> orders

  withGroup :: (OrderGroupId -> Handler a) -> Handler a
  withGroup handler = do
    groupId <- liftIO $ getOrderGroupId config (T.unpack groupKey)
    case groupId of
      Just id -> handler id
      _ -> throwError err404

commandServerV2 :: Config -> Text -> Server CommandApiV2
commandServerV2 config groupKey = 
       createOrderForHousehold
  :<|> createOrder
  :<|> ensureHouseholdOrderItem
  where
  createOrderForHousehold :: Int -> Handler Int
  createOrderForHousehold householdId = withGroup $ \groupId -> do
    date <- liftIO $ getCurrentTime
    orderId <- liftIO $ R.createOrder config groupId $ OrderSpec date $ Just $ HouseholdId householdId
    return $ fromOrderId orderId

  createOrder :: Handler Int
  createOrder = withGroup $ \groupId -> do
    date <- liftIO $ getCurrentTime
    orderId <- liftIO $ R.createOrder config groupId $ OrderSpec date Nothing
    return $ fromOrderId orderId
  
  ensureHouseholdOrderItem :: Int -> Int -> String -> Api.HouseholdOrderItemDetails -> Handler ()
  ensureHouseholdOrderItem orderId householdId productCode details = withGroup $ \groupId -> do
    date <- liftIO $ getCurrentTime
    -- liftIO $ D.ensureHouseholdOrderItem conn groupId orderId householdId productCode date details
    return ()

  withGroup :: (OrderGroupId -> Handler a) -> Handler a
  withGroup handler = do
    groupId <- liftIO $ getOrderGroupId config (T.unpack groupKey)
    case groupId of
      Just id -> handler id
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