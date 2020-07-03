{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as B (ByteString)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai (Application, responseFile)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Servant

import Api
import qualified Database as D
import Config
import AppServer
import AppServerV2
import CompareV2Api (compareApiV1WithApiV2)

main :: IO ()
main = do
  config <- getConfig
  setLocaleEncoding utf8
  run (Config.port config) $ app config

app :: Config -> Application
app config = logStdoutDev
           $ compareApiV1WithApiV2
           $ serve fullApi (server config)
         
server :: Config -> Server FullApi
server config = 
       groupServer config
  :<|> serveGroupPage
  :<|> serveDirectoryWebApp "client/static"

serveGroupPage :: Text -> Server Raw
serveGroupPage _ = Tagged (staticPolicy (addBase "client/static") indexPage)
  where
    indexPage :: Application
    indexPage _ respond = respond $ 
      responseFile status200
                   [(hContentType, "text/html")]
                   "client/static/index.html"
               Nothing

groupServer :: Config -> Server GroupApi
groupServer config groupKey = 
       verifyServer config groupKey
  :<|> appServerV2 config groupKey
  :<|> appServer config groupKey

verifyServer :: Config -> Text -> Server VerifyApi
verifyServer config groupKey = do
  group <- liftIO $ findGroup (connectionString config) groupKey
  case group of
    Just _ -> return True
    _ -> return False

findGroup :: B.ByteString -> Text -> IO (Maybe Int)
findGroup conn groupKey = do
  groupId <- D.getGroup conn (T.unpack groupKey)
  return groupId