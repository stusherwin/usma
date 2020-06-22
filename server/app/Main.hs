{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, putStrLn, length, take, putStr)
import           Data.ByteString.Builder (toLazyByteString, string8, lazyByteString)
import           Data.CaseInsensitive  (foldedCase)
import           Data.IORef (newIORef, modifyIORef', readIORef)
import           Data.List (intercalate, isInfixOf)
import           Data.Monoid ((<>))
import           Control.Exception (SomeException(..), catch, displayException)
import           Control.Monad (when, void)  
import           Control.Monad.Except (ExceptT(..))  
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as T (unpack, pack, concat, intercalate)
import           GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, utf8)
import           Network.HTTP.Types (hContentType, status200, statusMessage, statusCode)
import           Network.Wai (Application, Middleware, Request(..), Response, StreamingBody, responseFile, responseStatus, responseToStream)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Servant
import           System.Console.ANSI (Color(..), ConsoleLayer(..), ColorIntensity(..), SGR(..), setSGRCode)

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
           $ compareApiV1WithApiV2
           $ serve fullApi (server config)
         
server :: Config -> Server FullApi
server config = 
       groupServer config
  -- :<|> serveFilesWithGroup
  -- :<|> serveFiles
  -- :<|> serveFiles
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

compareApiV1WithApiV2 :: Middleware
compareApiV1WithApiV2 app req sendResponse = do
  if isApiV1 req
    then do
      let reqV2 = toApiV2 req
      app req $ \resp -> do
        respStr <- getResponseStr resp
        catch 
          (app reqV2 $ \respV2 -> do
            respStrV2 <- getResponseStr respV2
            compare (req, respStr) (reqV2, respStrV2)
            sendResponse resp)
          (\(SomeException ex) -> do
            compare (req, respStr) (reqV2, ("500 Server Error: \n    " ++ displayException ex, "", \() -> return $ BL.pack ""))
            sendResponse resp)
    else app req sendResponse
  where
    isApiV1 req = (pathInfo req) !! 2 == "v1"
    toApiV2 req = case pathInfo req of 
      ("api":g:"v1":rest) -> let pathInfo = "api":g:"v2":rest
                             in  req { pathInfo = pathInfo
                                     , rawPathInfo = B.pack $ T.unpack $ T.intercalate "/" $ pathInfo
                                     }
      _ -> req
    path req = T.unpack $ T.intercalate "/" $ pathInfo req
    compare (reqV1, (statusV1, headersV1, getBodyV1)) (reqV2, (statusV2, headersV2, getBodyV2)) = do
      if statusV1 /= statusV2 then do
        setColor Red
        putStrLn ""
        putStrLn $ "** V1/V2 Status mismatch ** -- " ++ (path reqV2)
        putStrLn $ "V1: " ++ statusV1
        putStrLn $ "V2: " ++ statusV2
        putStrLn ""
        resetColor
      else if headersV1 /= headersV2 then do
        setColor Red
        putStrLn ""
        putStrLn $ "** V1/V2 Header mismatch ** -- " ++ (path reqV2)
        putStrLn $ "V1: " ++ headersV1
        putStrLn $ "V2: " ++ headersV2
        putStrLn ""
        resetColor
      else do
        b1 <- getBodyV1 ()
        b2 <- getBodyV2 ()
        if b1 /= b2 then do
          setColor Red
          putStrLn ""
          putStrLn $ "** V1/V2 Body mismatch ** -- " ++ (path reqV2)
          putStr $ "V1: "
          BL.putStrLn b1
          putStr $ "V2: "
          BL.putStrLn b2
          putStrLn ""
          resetColor
        else do
          setColor Green
          putStrLn $ "V1/V2 OK -- " ++ (path reqV1)
          resetColor

getResponseStr :: Response -> IO (String, String, () -> IO BL.ByteString)
getResponseStr resp = do
    let (status, headers, body) = responseToStream resp
        statusStr = showStatus status
        headersStr = showHeaders headers
    return (statusStr, headersStr, if isImage headersStr
        then \() -> return . BL.pack $ "{image data}"
        else \() -> getResponseBody 1000 body)
  where
    isImage = isInfixOf "content-type: image/jpeg"
    showStatus status = (show . statusCode $ status) ++ " " ++ (B.unpack . statusMessage $ status)
    showHeaders headers = intercalate "," . map showHeader $ headers
    showHeader (header, value) = (B.unpack . foldedCase $ header) ++ ": " ++ (B.unpack value)

getResponseBody :: Int -> ((StreamingBody -> IO BL.ByteString) -> IO BL.ByteString) -> IO BL.ByteString
getResponseBody maxLen body = body $ \f -> do
  length <- newIORef (Just 0)
  content <- newIORef mempty
  f (\chunk -> readIORef length >>= \case
      Just len -> do
        let chunkBL = toLazyByteString chunk
        let chunkLen = fromIntegral $ BL.length chunkBL
        if len + chunkLen > maxLen
          then do
            let truncChunkBL = BL.take (fromIntegral $ maxLen - len) chunkBL
            modifyIORef' content (<> (lazyByteString truncChunkBL))
            modifyIORef' content (<> (string8 "..."))
            modifyIORef' length (const Nothing)
          else do
            modifyIORef' content (<> chunk)
            modifyIORef' length $ fmap (+ chunkLen)
      _ -> return ())
    (return ())
  toLazyByteString <$> readIORef content

setColor :: Color -> IO ()
setColor color = BL.putStr $ BL.pack $ setSGRCode [SetColor Foreground Dull color]

resetColor :: IO ()
resetColor = BL.putStr $ BL.pack $ setSGRCode [Reset]