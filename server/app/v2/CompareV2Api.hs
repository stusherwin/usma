{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module CompareV2Api where

import           Data.Aeson (Object, decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack, unpack, null, empty)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, putStr, writeFile)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.CaseInsensitive  (foldedCase)
import           Data.IORef (newIORef, modifyIORef', readIORef)
import           Data.List (intercalate, isInfixOf)
import           Data.Monoid ((<>))
import           Data.UUID (toString)
import           Data.UUID.V1 (nextUUID)
import           Control.Exception (SomeException(..), catch, displayException)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T (unpack, intercalate)
import           Network.HTTP.Types (statusMessage, statusCode)
import           Network.Wai (Middleware, Request(..), Response, StreamingBody, responseToStream)
import           System.Console.ANSI (Color(..), ConsoleLayer(..), ColorIntensity(..), SGR(..), setSGRCode)
import           System.Command (readProcessWithExitCode)
import           System.Directory (createDirectoryIfMissing, removeFile)

type ResponseInfo = (String, String, IO BL.ByteString)

compareApiV1WithApiV2 :: Middleware
compareApiV1WithApiV2 app req sendResponse = do
  if isApiV1 req
    then do
      chunks <- getRequestBodyChunks req
      reqV1 <- withRequestBodyChunks chunks req
      reqV2 <- toApiV2 <$> withRequestBodyChunks chunks req
      app reqV1 $ \resp -> do
        respInfoV1 <- getResponseInfo resp
        catch 
          (app reqV2 $ \respV2 -> do
            respInfoV2 <- getResponseInfo respV2
            compareResponses (path reqV2) respInfoV1 respInfoV2
            sendResponse resp)
          (\(SomeException ex) -> do
            let exceptionInfo = ("500 Server Error: \n    " ++ displayException ex, "", return $ BL.pack "")
            compareResponses (path reqV2) respInfoV1 exceptionInfo
            sendResponse resp)
    else app req sendResponse

isApiV1 :: Request -> Bool
isApiV1 req = length (pathInfo req) > 2 && (pathInfo req) !! 2 == "v1"

toApiV2 :: Request -> Request
toApiV2 req = case pathInfo req of 
  ("api":g:"v1":rest) -> let pi = "api":g:"v2":rest
                         in  req{ pathInfo = pi
                                , rawPathInfo = B.pack $ T.unpack $ T.intercalate "/" pi
                                }
  _ -> req

getRequestBodyChunks :: Request -> IO [B.ByteString]
getRequestBodyChunks req = do
    chunksRef <- newIORef []
    nextChunk chunksRef
    readIORef chunksRef
  where
    nextChunk chunksRef = do
      chunk <- requestBody req
      if (not . B.null) chunk 
        then do
          modifyIORef' chunksRef (chunk :)
          nextChunk chunksRef
        else return ()

withRequestBodyChunks :: [B.ByteString] -> Request -> IO Request
withRequestBodyChunks chunks req = do
  reqBody <- getRequestBody chunks
  return req{ requestBody = reqBody }

getRequestBody :: [B.ByteString] -> IO (IO B.ByteString)
getRequestBody chunks = do
    chunksRef <- newIORef chunks
    return $ getNextChunk chunksRef
  where
    getNextChunk chunksRef = do
      chunks <- readIORef chunksRef
      case chunks of
        (c:cs) -> do
          modifyIORef' chunksRef (const cs)
          return c
        _ -> return B.empty

getResponseInfo :: Response -> IO ResponseInfo
getResponseInfo resp = do
    let (status, headers, body) = responseToStream resp
        statusStr = showStatus status
        headersStr = showHeaders headers
    return (statusStr, headersStr, 
      if isImage headersStr
        then return . BL.pack $ "{image data}"
        else getResponseBody body
      )
  where
    isImage = isInfixOf "content-type: image/jpeg"
    showStatus status = (show . statusCode $ status) ++ " " ++ (B.unpack . statusMessage $ status)
    showHeaders headers = intercalate "," . map showHeader $ headers
    showHeader (header, value) = (B.unpack . foldedCase $ header) ++ ": " ++ (B.unpack value)

getResponseBody :: ((StreamingBody -> IO BL.ByteString) -> IO BL.ByteString) -> IO BL.ByteString
getResponseBody body = body $ \f -> do
  content <- newIORef mempty
  f (\chunk -> modifyIORef' content (<> chunk)) (return ())
  toLazyByteString <$> readIORef content

path :: Request -> String
path req = T.unpack $ T.intercalate "/" $ pathInfo req

compareResponses :: String -> ResponseInfo -> ResponseInfo -> IO ()
compareResponses path (statusV1, headersV1, getBodyV1) (statusV2, headersV2, getBodyV2) = do
  if statusV1 /= statusV2 then do
    setColor Red
    putStrLn ""
    putStrLn $ "** V1/V2 Status mismatch ** -- " ++ path
    putStrLn $ "V1: " ++ statusV1
    putStrLn $ "V2: " ++ statusV2
    putStrLn ""
    resetColor
  else if headersV1 /= headersV2 then do
    setColor Red
    putStrLn ""
    putStrLn $ "** V1/V2 Header mismatch ** -- " ++ path
    putStrLn $ "V1: " ++ headersV1
    putStrLn $ "V2: " ++ headersV2
    putStrLn ""
    resetColor
  else do
    bodyV1 <- getBodyV1
    bodyV2 <- getBodyV2
    if bodyV1 /= bodyV2 then do
      setColor Red
      putStrLn ""
      putStrLn $ "** V1/V2 Body mismatch ** -- " ++ path
      resetColor
      showDiff bodyV1 bodyV2
      putStrLn ""
    else do
      setColor Green
      putStrLn $ "V1/V2 OK -- " ++ path
      resetColor

showDiff :: BL.ByteString -> BL.ByteString -> IO ()
showDiff v1 v2 = do
  diffId <- fmap toString <$> liftIO nextUUID
  case diffId of
    Nothing -> return ()
    Just diffId -> do
      let diffDir = "server/data/diffs/"
      liftIO $ createDirectoryIfMissing True diffDir
      let v1File = diffDir ++ diffId ++ "-v1.txt"
      let v2File = diffDir ++ diffId ++ "-v2.txt"
      let objV1 = decode v1 :: Maybe Object
      BL.writeFile v1File $ case objV1 of
        Just obj -> encodePretty obj
        _ -> v1
      let objV2 = decode v2 :: Maybe Object
      BL.writeFile v2File $ case objV2 of
        Just obj -> encodePretty obj
        _ -> v2
      (_, out, _) <- readProcessWithExitCode "git" 
        [ "diff"
        , "--no-index"
        , "--word-diff=color"
        -- , "--inter-hunk-context=999"
        -- , "--function-context" 
        , v1File
        , v2File
        ] ""
      putStrLn out
      removeFile v1File
      removeFile v2File

setColor :: Color -> IO ()
setColor color = BL.putStr $ BL.pack $ setSGRCode [SetColor Foreground Dull color]

resetColor :: IO ()
resetColor = BL.putStr $ BL.pack $ setSGRCode [Reset]