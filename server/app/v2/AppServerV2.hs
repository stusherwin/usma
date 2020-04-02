module AppServerV2 (appServerV2) where

import Servant
import Servant.Multipart

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text as T (unpack)
import qualified Data.ByteString as B (ByteString)

import Config

import AppApiV2
import DomainV2
import RepositoryV2

appServerV2 :: Config -> Text -> Server AppApiV2
appServerV2 config groupKey =
  queryServerV2 config groupKey
  where
  conn = connectionString config

queryServerV2 :: Config -> Text -> Server QueryApiV2
queryServerV2 config groupKey = 
       collectiveOrder groupKey
  where
  collectiveOrder :: Text -> Handler (Maybe CollectiveOrder)
  collectiveOrder groupKey = findGroupOr404 config groupKey $ \groupId -> do
    order <- liftIO $ getOrder config groupId
    return $ order <&> \o -> CollectiveOrder
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
      , coItems = _orderItems o <&> \i -> AppApiV2.OrderItem
        { oiProductId          = fromProductId . _productId . _itemProduct $ i 
        , oiProductCode        = _productCode    . _itemProduct $ i
        , oiProductName        = _productName    . _itemProduct $ i
        , oiProductVatRate     = _productVatRate . _itemProduct $ i
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
      }

findGroupOr404 :: Config -> Text -> (OrderGroupId -> Handler a) -> Handler a
findGroupOr404 conn groupKey handler = do
  groupId <- liftIO $ getOrderGroupId conn (T.unpack groupKey)
  case groupId of
    Just id -> handler id
    _ -> throwError err404

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &