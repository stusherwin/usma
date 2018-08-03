{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  
  import Control.Monad.IO.Class (liftIO)
  import Network.Wai.Handler.Warp (run)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
  import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
  import Data.Time.Calendar (toGregorian, fromGregorian, addGregorianYearsClip)
  import Data.Time.Format (formatTime, defaultTimeLocale)
  import Data.Text (Text)
  import Text.Read (readMaybe)
  import qualified Data.Text as T (pack, unpack)
  import qualified Data.ByteString.Char8 as B (pack, unpack)
  import Servant
  import Control.Concurrent(threadDelay)
  import Data.ByteString (ByteString)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import System.Environment (getEnv, getArgs)
  import Web.Cookie (parseCookiesText)
  import Web.HttpApiData (parseUrlPiece, toUrlPiece)
  import Data.Time.Calendar (Day)
  
  import Types 
  import Api
  import qualified Database as D

  main :: IO ()
  main = do
    args <- getArgs
    let portStr = head args
    putStrLn $ "port: " ++ portStr
    connectionString <- if length args > 1 then return (args !! 1)
                                           else getEnv "DATABASE_URL"
    putStrLn $ "connection string: " ++ connectionString
    run (read portStr) $ app $ B.pack connectionString

  app :: ByteString -> Application
  app conn = logStdoutDev
             $ serve fullAPI (server conn)
           
  server :: ByteString -> Server FullAPI
  server conn = appServer conn
           :<|> serveDirectoryFileServer "client/static"

  appServer :: ByteString -> Server AppAPI
  appServer conn = queryServer conn
              :<|> commandServer conn 
  
  queryServer :: ByteString -> Server QueryAPI
  queryServer conn = orders
                :<|> orderSummary 
    where
    orders :: Handler [Order]
    orders = liftIO $ D.getAllOrders conn

    orderSummary :: Day -> Handler OrderSummary
    orderSummary day = do
      result <- liftIO $ D.getOrderSummary conn day
      case result of
        Just v -> return v
        _ -> throwError err404
  
  commandServer :: ByteString -> Server CommandAPI
  commandServer conn = createOrder
    where
    createOrder :: Day -> Handler ()
    createOrder = liftIO . (D.createOrder conn)