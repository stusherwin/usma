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
  collectiveOrder groupKey = findGroupOr404 conn groupKey $ \groupId -> return Nothing

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