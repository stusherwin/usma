{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App where
 
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B (ByteString)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Database.PostgreSQL.Simple (connectPostgreSQL, fromOnly)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai (responseFile)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Network.Wai.Parse (clearMaxHeaderLines, clearMaxHeaderLineLength, defaultParseRequestBodyOptions)
import           Servant
import           Servant.Multipart (generalOptions, defaultMultipartOptions, Mem)

import Api
import Config
import qualified V2.Domain as V2 (fromOrderGroupId)
import qualified V2.Server as V2 (server)
import qualified V2.Repository.SQL as V2 (selectOrderGroupId)

app :: Config -> Application
app config = serveWithContext fullApi ctxt (server config)
  where ctxt = multipartOpts :. EmptyContext
        multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Mem))
          { generalOptions = clearMaxHeaderLines $
                             clearMaxHeaderLineLength $
                             defaultParseRequestBodyOptions 
          }
         
server :: Config -> Server Api
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
  :<|> V2.server config groupKey

verifyServer :: Config -> Text -> Server VerifyApi
verifyServer config groupKey = do
  group <- liftIO $ findGroup (connectionString config) groupKey
  case group of
    Just _ -> return True
    _ -> return False

findGroup :: B.ByteString -> Text -> IO (Maybe Int)
findGroup connectionString groupKey = do
  conn <- liftIO $ connectPostgreSQL connectionString
  listToMaybe . (fmap (V2.fromOrderGroupId . fromOnly)) <$> V2.selectOrderGroupId conn (T.unpack groupKey)