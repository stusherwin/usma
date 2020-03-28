module AppServerV2 (appServerV2) where

import Servant
import Servant.Multipart

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text as T (unpack)
import qualified Data.ByteString as B (ByteString)

import qualified Database as D
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
  conn = connectionString config

  collectiveOrder :: Text -> Handler (Maybe CollectiveOrder)
  collectiveOrder groupKey = findGroupOr404 conn groupKey $ \groupId -> do
    order <- liftIO $ getCollectiveOrder config groupId
    return $ order <&> \o -> CollectiveOrder
      { coId                    = fromOrderId . orderId $ o
      , coOrderCreatedDate      = orderCreated o
      , coOrderCreatedBy        = fmap fromHouseholdId . fmap householdId . orderCreatedBy $ o
      , coOrderCreatedByName    = fmap householdName . orderCreatedBy $ o
      , coOrderIsPlaced         = orderIsPlaced o
      , coOrderIsAbandoned      = orderIsAbandoned o
      , coIsComplete            = orderIsComplete o
      , coTotalExcVat           = excVat . orderTotal $ o
      , coTotalIncVat           = incVat . orderTotal $ o
      , coAllHouseholdsUpToDate = orderIsAllHouseholdsUpToDate o
      , coAdjustment = Nothing -- todo
      , coItems = orderItems o <&> \i -> AppApiV2.OrderItem
        { oiProductId          = fromProductId . productId . itemProduct $ i 
        , oiProductCode        = productCode . itemProduct $ i
        , oiProductName        = productName . itemProduct $ i
        , oiProductVatRate     = productVatRate . itemProduct $ i
        , oiProductPriceExcVat = excVat . productPrice . itemProduct $ i
        , oiProductPriceIncVat = incVat . productPrice . itemProduct $ i
        , oiItemQuantity       = itemQuantity i
        , oiItemTotalExcVat    = excVat . itemTotal $ i
        , oiItemTotalIncVat    = incVat . itemTotal $ i
        , oiBiodynamic         = productIsBiodynamic . itemProduct $ i
        , oiFairTrade          = productIsFairTrade . itemProduct $ i
        , oiGlutenFree         = productIsGlutenFree . itemProduct $ i
        , oiOrganic            = productIsOrganic . itemProduct $ i
        , oiAddedSugar         = productIsAddedSugar . itemProduct $ i
        , oiVegan              = productIsVegan . itemProduct $ i
        , oiAdjustment         = Nothing -- todo
        }
      }

findGroupOr404 :: B.ByteString -> Text -> (Int -> Handler a) -> Handler a
findGroupOr404 conn groupKey handler = do
  groupId <- liftIO $ findGroup conn groupKey
  case groupId of
    Just id -> handler id
    _ -> throwError err404

findGroup :: B.ByteString -> Text -> IO (Maybe Int)
findGroup conn groupKey = do
  rotaId <- D.getGroup conn (T.unpack groupKey)
  return rotaId

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
