module AppServerV2 (appServerV2) where

import Servant
import Servant.Multipart

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text as T (unpack)
import qualified Data.ByteString as B (ByteString)

import Config

import AppApiV2 as Api
import DomainV2
import RepositoryV2

appServerV2 :: Config -> Text -> Server Api.AppApiV2
appServerV2 config groupKey =
  queryServerV2 config groupKey
  where
  conn = connectionString config

queryServerV2 :: Config -> Text -> Server Api.QueryApiV2
queryServerV2 config groupKey = 
       households groupKey
  :<|> collectiveOrder groupKey
  :<|> pastCollectiveOrders groupKey
  where
  households :: Text -> Handler [Api.Household]
  households groupKey = findGroupOr404 config groupKey $ \groupId -> do
    households <- liftIO $ getHouseholds config groupId
    return $ apiHousehold <$> households

  collectiveOrder :: Text -> Handler (Maybe Api.CollectiveOrder)
  collectiveOrder groupKey = findGroupOr404 config groupKey $ \groupId -> do
    order <- liftIO $ getOrder config groupId
    return $ apiOrder <$> order

  pastCollectiveOrders :: Text -> Handler [Api.CollectiveOrder]
  pastCollectiveOrders groupKey = findGroupOr404 config groupKey $ \groupId -> do
    orders <- liftIO $ getPastOrders config groupId
    return $ apiOrder <$> orders

apiHousehold :: DomainV2.Household -> Api.Household 
apiHousehold h = Api.Household
  { hId = fromHouseholdId . _householdId . _householdInfo $ h
  , hName = _householdName . _householdInfo $ h
  , hContactName = _householdContactName h
  , hContactEmail = _householdContactEmail h
  , hContactPhone = _householdContactPhone h
  , hTotalOrders = _householdTotalOrders h
  , hTotalPayments = _householdTotalPayments h
  , hBalance = _householdBalance h
  }

apiOrder :: Order -> Api.CollectiveOrder
apiOrder o = Api.CollectiveOrder
  { coId                    = fromOrderId . _orderId . _orderInfo $ o
  , coOrderCreatedDate      = _orderCreated . _orderInfo $ o
  , coOrderCreatedBy        = fmap fromHouseholdId . fmap _householdId . _orderCreatedBy . _orderInfo $ o
  , coOrderCreatedByName    = fmap _householdName . _orderCreatedBy . _orderInfo $ o
  , coOrderIsPlaced         = orderIsPlaced o
  , coOrderIsAbandoned      = orderIsAbandoned o
  , coIsComplete            = orderIsComplete o
  , coAllHouseholdsUpToDate = orderIsAllHouseholdsUpToDate o
  , coTotalExcVat           = _excVat . _orderTotal $ o
  , coTotalIncVat           = _incVat . _orderTotal $ o
  , coAdjustment            = undefined
  , coItems = apiOrderItem <$> _orderItems o
  }

apiOrderItem :: DomainV2.OrderItem -> Api.OrderItem
apiOrderItem i = Api.OrderItem
  { oiProductId          = fromProductId . _productId . _itemProduct $ i 
  , oiProductCode        = _productCode    . _itemProduct $ i
  , oiProductName        = _productName    . _itemProduct $ i
  , oiProductVatRate     = _type . _productVatRate . _itemProduct $ i
  , oiProductPriceExcVat = _excVat . _productPrice . _itemProduct $ i
  , oiProductPriceIncVat = _incVat . _productPrice . _itemProduct $ i
  , oiItemQuantity       = _itemQuantity i
  , oiItemTotalExcVat    = _excVat . _itemTotal $ i
  , oiItemTotalIncVat    = _incVat . _itemTotal $ i
  , oiBiodynamic         = _productAttrIsBiodynamic . _productAttributes . _itemProduct $ i
  , oiFairTrade          = _productAttrIsFairTrade  . _productAttributes . _itemProduct $ i
  , oiGlutenFree         = _productAttrIsGlutenFree . _productAttributes . _itemProduct $ i
  , oiOrganic            = _productAttrIsOrganic    . _productAttributes . _itemProduct $ i
  , oiAddedSugar         = _productAttrIsAddedSugar . _productAttributes . _itemProduct $ i
  , oiVegan              = _productAttrIsVegan      . _productAttributes . _itemProduct $ i
  , oiAdjustment         = undefined
  }

findGroupOr404 :: Config -> Text -> (OrderGroupId -> Handler a) -> Handler a
findGroupOr404 conn groupKey handler = do
  groupId <- liftIO $ getOrderGroupId conn (T.unpack groupKey)
  case groupId of
    Just id -> handler id
    _ -> throwError err404