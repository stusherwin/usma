{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App where
 
import qualified Data.ByteString as B (ByteString)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import           Network.HTTP.Types (hContentType, status200)
import           Network.Wai (responseFile)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Servant.Multipart (generalOptions, defaultMultipartOptions, Mem)
import           Network.Wai.Parse (clearMaxHeaderLines, clearMaxHeaderLineLength, defaultParseRequestBodyOptions)
import           Servant

import Api
import qualified Database as D
import Config
import AppServer
import AppServerV2
import qualified ProductImage as V1 (FetchProductImage)
import qualified SumaCatalogue as V2 (FetchProductImage)

app :: V1.FetchProductImage -> V2.FetchProductImage -> Config -> Application
app fetchProductImageV1 fetchProductImageV2 config = serveWithContext fullApi ctxt (server fetchProductImageV1 fetchProductImageV2 config)
  where ctxt = multipartOpts :. EmptyContext
        multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Mem))
          { generalOptions = clearMaxHeaderLines $
                             clearMaxHeaderLineLength $
                             defaultParseRequestBodyOptions 
          }
         
server :: V1.FetchProductImage -> V2.FetchProductImage -> Config -> Server FullApi
server fetchProductImageV1 fetchProductImageV2 config = 
       groupServer fetchProductImageV1 fetchProductImageV2 config
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

groupServer :: V1.FetchProductImage -> V2.FetchProductImage -> Config -> Server GroupApi
groupServer fetchProductImageV1 fetchProductImageV2 config groupKey = 
       verifyServer config groupKey
  :<|> appServerV2 fetchProductImageV2 config groupKey
  :<|> appServer fetchProductImageV1 config groupKey

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