module AppServerV2 (appServerV2) where

import Servant
import Servant.Multipart

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text as T (unpack)
import qualified Data.ByteString as B (ByteString)

import qualified Types as TypesV1
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
      { coId                    = fromOrderId . orderId . orderInfo $ o
      , coOrderCreatedDate      = orderCreated . orderInfo $ o
      , coOrderCreatedBy        = fmap fromHouseholdId . fmap householdId . orderCreatedBy . orderInfo $ o
      , coOrderCreatedByName    = fmap householdName . orderCreatedBy . orderInfo $ o
      , coOrderIsPlaced         = case orderStatus o of
                                    OrderPlaced -> True
                                    _           -> False
      , coOrderIsAbandoned      = case orderStatus o of
                                    OrderAbandoned -> True
                                    _              -> False
      , coIsComplete            = case orderStatus o of
                                    OrderComplete -> True
                                    _              -> False
      , coAllHouseholdsUpToDate = case orderStatus o of
                                    OrderAwaitingHouseholdsUpdateConfirm -> False
                                    _                                    -> True
      , coTotalExcVat           = excVat . orderTotal $ o
      , coTotalIncVat           = incVat . orderTotal $ o
      , coAdjustment            = undefined
      , coItems = orderItems o <&> \i -> AppApiV2.OrderItem
        { oiProductId          = fromProductId . productId . itemProduct $ i 
        , oiProductCode        = productCode    . itemProduct $ i
        , oiProductName        = productName    . itemProduct $ i
        , oiProductVatRate     = productVatRate . itemProduct $ i
        , oiProductPriceExcVat = excVat . productPrice . itemProduct $ i
        , oiProductPriceIncVat = incVat . productPrice . itemProduct $ i
        , oiItemQuantity       = itemQuantity i
        , oiItemTotalExcVat    = excVat . itemTotal $ i
        , oiItemTotalIncVat    = incVat . itemTotal $ i
        , oiBiodynamic         = productAttrIsBiodynamic . productAttributes . itemProduct $ i
        , oiFairTrade          = productAttrIsFairTrade  . productAttributes . itemProduct $ i
        , oiGlutenFree         = productAttrIsGlutenFree . productAttributes . itemProduct $ i
        , oiOrganic            = productAttrIsOrganic    . productAttributes . itemProduct $ i
        , oiAddedSugar         = productAttrIsAddedSugar . productAttributes . itemProduct $ i
        , oiVegan              = productAttrIsVegan      . productAttributes . itemProduct $ i
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
