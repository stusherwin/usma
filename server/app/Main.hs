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
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment

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
  queryServer conn = collectiveOrders
                :<|> householdOrders
                :<|> products
                :<|> households
                :<|> householdPayments
    where
    collectiveOrders :: Handler [CollectiveOrder]
    collectiveOrders = liftIO $ D.getCollectiveOrders conn
    
    householdOrders :: Handler [HouseholdOrder]
    householdOrders = liftIO $ D.getHouseholdOrders conn
    
    products :: Handler [Product]
    products = liftIO $ D.getProducts conn

    households :: Handler [Household]
    households = liftIO $ D.getHouseholds conn

    householdPayments :: Handler [HouseholdPayment]
    householdPayments = liftIO $ D.getHouseholdPayments conn

  commandServer :: ByteString -> Server CommandAPI
  commandServer conn = createOrderForHousehold
                  :<|> createOrder
                  :<|> archiveOrder
                  :<|> addHouseholdOrder
                  :<|> cancelHouseholdOrder
                  :<|> completeHouseholdOrder
                  :<|> reopenHouseholdOrder
                  :<|> ensureHouseholdOrderItem
                  :<|> removeHouseholdOrderItem
                  :<|> createHousehold
                  :<|> updateHousehold
                  :<|> archiveHousehold
                  :<|> createProduct
                  :<|> updateProduct
                  :<|> archiveProduct
                  :<|> createHouseholdPayment
                  :<|> updateHouseholdPayment
                  :<|> archiveHouseholdPayment
    where
    createOrderForHousehold :: Int -> Handler Int
    createOrderForHousehold householdId = do
      day <- liftIO $ getCurrentTime >>= return . utctDay      
      liftIO $ D.createOrder conn day (Just householdId)

    createOrder :: Handler Int
    createOrder = do
      day <- liftIO $ getCurrentTime >>= return . utctDay      
      liftIO $ D.createOrder conn day Nothing

    archiveOrder :: Int -> Handler ()
    archiveOrder = liftIO . (D.archiveOrder conn)

    addHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    addHouseholdOrder command = liftIO $ D.addHouseholdOrder conn (choOrderId command) (choHouseholdId command)

    cancelHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    cancelHouseholdOrder command = liftIO $ D.cancelHouseholdOrder conn (choOrderId command) (choHouseholdId command)

    completeHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    completeHouseholdOrder command = liftIO $ D.completeHouseholdOrder conn (choOrderId command) (choHouseholdId command)

    reopenHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    reopenHouseholdOrder command = liftIO $ D.reopenHouseholdOrder conn (choOrderId command) (choHouseholdId command)
 
    ensureHouseholdOrderItem :: EnsureHouseholdOrderItem -> Handler ()
    ensureHouseholdOrderItem command = liftIO $ D.ensureHouseholdOrderItem conn (ehoiOrderId command) (ehoiHouseholdId command) (ehoiProductId command) (ehoiQuantity command)

    removeHouseholdOrderItem :: RemoveHouseholdOrderItem -> Handler ()
    removeHouseholdOrderItem command = liftIO $ D.removeHouseholdOrderItem conn (rhoiOrderId command) (rhoiHouseholdId command) (rhoiProductId command)

    createHousehold :: CreateHousehold -> Handler Int
    createHousehold command = liftIO $ D.createHousehold conn (chName command)

    updateHousehold :: UpdateHousehold -> Handler ()
    updateHousehold command = liftIO $ D.updateHousehold conn (uhId command) (uhName command)

    archiveHousehold :: Int -> Handler ()
    archiveHousehold = liftIO . (D.archiveHousehold conn)

    createProduct :: CreateProduct -> Handler Int
    createProduct command = liftIO $ D.createProduct conn (cpName command) (cpPrice command)

    updateProduct :: UpdateProduct -> Handler ()
    updateProduct command = liftIO $ D.updateProduct conn (upId command) (upName command) (upPrice command)

    archiveProduct :: Int -> Handler ()
    archiveProduct = liftIO . (D.archiveProduct conn)
  
    createHouseholdPayment :: CreateHouseholdPayment -> Handler Int
    createHouseholdPayment command = liftIO $ D.createHouseholdPayment conn (chpHouseholdId command) (chpDate command) (chpAmount command)

    updateHouseholdPayment :: UpdateHouseholdPayment -> Handler ()
    updateHouseholdPayment command = liftIO $ D.updateHouseholdPayment conn (uhpPaymentId command) (uhpDate command) (uhpAmount command)

    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment = liftIO . (D.archiveHouseholdPayment conn)
