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
import qualified V1.Server as V1 (server)
import qualified V1.ProductImage as V1 (FetchProductImage)
import qualified V2.Domain as V2 (fromOrderGroupId)
import qualified V2.Server as V2 (server)
import qualified V2.SumaCatalogue as V2 (FetchProductImage)
import qualified V2.Repository.SQL as V2 (selectOrderGroupId)

app :: V1.FetchProductImage -> V2.FetchProductImage -> Config -> Application
app fetchProductImageV1 fetchProductImageV2 config = serveWithContext fullApi ctxt (server fetchProductImageV1 fetchProductImageV2 config)
  where ctxt = multipartOpts :. EmptyContext
        multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Mem))
          { generalOptions = clearMaxHeaderLines $
                             clearMaxHeaderLineLength $
                             defaultParseRequestBodyOptions 
          }
         
server :: V1.FetchProductImage -> V2.FetchProductImage -> Config -> Server Api
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
  :<|> V2.server fetchProductImageV2 config groupKey
  :<|> V1.server fetchProductImageV1 config groupKey

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