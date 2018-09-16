{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  
  import Control.Monad.IO.Class (liftIO)
  import Network.Wai.Handler.Warp (run)
  import Network.Wai (responseFile)
  import Network.HTTP.Types (hContentType, status200)
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
  import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
  import Data.Time.Calendar (toGregorian, fromGregorian, addGregorianYearsClip)
  import Data.Time.Format (formatTime, defaultTimeLocale)
  import Servant
  import Control.Concurrent(threadDelay)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import Network.Wai.Middleware.Static (staticPolicy, addBase)
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
  import Config

  main :: IO ()
  main = do
    config <- getConfig
    run (port config) $ app config

  app :: Config -> Application
  app config = logStdoutDev
               $ serve fullAPI (server config)
           
  server :: Config -> Server FullAPI
  server config = appServer config
             :<|> Tagged (staticPolicy (addBase "client/static") staticOrDefault)

  staticOrDefault :: Application
  staticOrDefault req respond = respond $ 
    responseFile
    status200
    [(hContentType, "text/html")]
    "client/static/index.html"
    Nothing

  appServer :: Config -> Server AppAPI
  appServer config = queryServer config
                :<|> commandServer config 
  
  queryServer :: Config -> Server QueryAPI
  queryServer config = collectiveOrders
                  :<|> householdOrders
                  :<|> products
                  :<|> households
                  :<|> householdPayments
                  :<|> productList
    where
    conn = connectionString config
    
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

    productList :: Handler [Product]
    productList = liftIO $ loadProductList config
      
  commandServer :: Config -> Server CommandAPI
  commandServer config = createOrderForHousehold
                    :<|> createOrder
                    :<|> deleteOrder
                    :<|> placeOrder
                    :<|> createHouseholdOrder
                    :<|> deleteHouseholdOrder
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
    conn = connectionString config
    
    createOrderForHousehold :: Int -> Handler Int
    createOrderForHousehold householdId = do
      day <- liftIO $ getCurrentTime >>= return . utctDay      
      liftIO $ D.createOrder conn day (Just householdId)

    createOrder :: Handler Int
    createOrder = do
      day <- liftIO $ getCurrentTime >>= return . utctDay      
      liftIO $ D.createOrder conn day Nothing

    deleteOrder :: Int -> Handler ()
    deleteOrder = liftIO . (D.deleteOrder conn)

    placeOrder :: Int -> Handler ()
    placeOrder = liftIO . (D.placeOrder conn)

    createHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    createHouseholdOrder command = liftIO $ D.createHouseholdOrder conn (choOrderId command) (choHouseholdId command)

    deleteHouseholdOrder :: CancelHouseholdOrder -> Handler ()
    deleteHouseholdOrder command = liftIO $ D.deleteHouseholdOrder conn (choOrderId command) (choHouseholdId command)

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
    createProduct command = liftIO $ D.createProduct conn (cpCode command) (cpName command) (cpPrice command) (cpVatRate command)

    updateProduct :: UpdateProduct -> Handler ()
    updateProduct command = liftIO $ D.updateProduct conn (upId command) (upCode command) (upName command) (upPrice command) (upVatRate command)

    archiveProduct :: Int -> Handler ()
    archiveProduct = liftIO . (D.archiveProduct conn)
  
    createHouseholdPayment :: CreateHouseholdPayment -> Handler Int
    createHouseholdPayment command = liftIO $ D.createHouseholdPayment conn (chpHouseholdId command) (chpDate command) (chpAmount command)

    updateHouseholdPayment :: UpdateHouseholdPayment -> Handler ()
    updateHouseholdPayment command = liftIO $ D.updateHouseholdPayment conn (uhpPaymentId command) (uhpDate command) (uhpAmount command)

    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment = liftIO . (D.archiveHouseholdPayment conn)
