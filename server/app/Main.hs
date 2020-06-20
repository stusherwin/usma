{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Monad.Except (ExceptT(..))  
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B (ByteString)
import           Data.Text (Text)
import           Data.Text as T (unpack)
import           GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai (responseFile)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Servant

import Api
import qualified Database as D
import Config
import ProductImage
import AppServer
import AppServerV2

main :: IO ()
main = do
  config <- getConfig
  setLocaleEncoding utf8
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