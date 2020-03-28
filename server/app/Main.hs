{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where
  
import Servant
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseFile)
import Network.HTTP.Types (hContentType, status200)
import Data.Text (Text)
import Data.Text as T (unpack)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.ByteString as B (ByteString)

import Api
import qualified Database as D
import Config

import AppServer
import AppServerV2

main :: IO ()
main = do
  config <- getConfig
  run (Config.port config) $ app config

app :: Config -> Application
app config = logStdoutDev
             $ serve fullApi (server config)
         
server :: Config -> Server FullApi
server config = 
       groupServer config
  :<|> serveFilesWithGroup
  :<|> serveFiles
  :<|> serveFiles
  where
  serveFilesWithGroup :: Text -> Server Raw
  serveFilesWithGroup _ = serveFiles
  
  serveFiles :: Server Raw
  serveFiles = Tagged (staticPolicy (addBase "client/static") staticOrDefault)

staticOrDefault :: Application
staticOrDefault req respond = respond $ 
  responseFile
  status200
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