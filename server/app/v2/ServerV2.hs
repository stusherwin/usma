module ServerV2 (appServerV2) where

import Servant
import Servant.Multipart

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseFile)
import Network.HTTP.Types (hContentType, status200)
import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar (toGregorian, fromGregorian, addGregorianYearsClip)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Text (Text)
import Data.List (find)
import Data.Text as T (unpack)
import Control.Concurrent(threadDelay)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Cookie (parseCookiesText)
import Web.HttpApiData (parseUrlPiece, toUrlPiece)
import Data.Time.Calendar (Day)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import Control.Monad (mzero, when, void)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict, unpack, writeFile, readFile)
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import Data.Csv as Csv (encode, ToNamedRecord(..), (.=), namedRecord, encodeByName)
import Data.Vector as V (fromList)
import Network.HTTP.Conduit ()
import Text.HTML.TagSoup ()
import Data.Text.Encoding ()
import System.IO (hFlush, stdout)
import Control.Exception (handle, SomeException(..))

import qualified Database as D
import Config

import Api
import ApiV2
import DomainV2

appServerV2 :: Config -> Server AppApiV2
appServerV2 config = 
       verifyServer config
  :<|> withGroupServerV2 config

withGroupServerV2 :: Config -> Server WithGroupApiV2
withGroupServerV2 config groupKey = 
       queryServerV2 config groupKey

queryServerV2 :: Config -> Text -> Server QueryApiV2
queryServerV2 config groupKey = 
       collectiveOrder groupKey
  where
  conn = connectionString config
  
  collectiveOrder :: Text -> Handler (Maybe Int)
  collectiveOrder groupKey = findGroupOr404 conn groupKey $ \groupId ->
    return Nothing

verifyServer :: Config -> Server VerifyApi
verifyServer config groupKey = do
  group <- liftIO $ findGroup (connectionString config) groupKey
  case group of
    Just _ -> return True
    _ -> return False

findGroup :: B.ByteString -> Text -> IO (Maybe Int)
findGroup conn groupKey = do
  rotaId <- D.getGroup conn (T.unpack groupKey)
  return rotaId

findGroupOr404 :: B.ByteString -> Text -> (Int -> Handler a) -> Handler a
findGroupOr404 conn groupKey handler = do
  groupId <- liftIO $ findGroup conn groupKey
  case groupId of
    Just id -> handler id
    _ -> throwError err404