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
  import Data.Text (Text, unpack)
  import Servant
  import Servant.Multipart
  import Control.Concurrent(threadDelay)
  import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, simpleMethods, corsOrigins)
  import Network.Wai.Middleware.Servant.Options (provideOptions)
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import Network.Wai.Middleware.Static (staticPolicy, addBase)
  import Web.Cookie (parseCookiesText)
  import Web.HttpApiData (parseUrlPiece, toUrlPiece)
  import Data.Time.Calendar (Day)
  import System.Directory (copyFile)
  
  import Api
  import qualified Database as D
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment
  import Config
  import ProductRepository

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
                    :<|> unplaceOrder
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
                    :<|> uploadProductList
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
    deleteOrder orderId = liftIO $ D.deleteOrder conn orderId

    placeOrder :: Int -> Handler ()
    placeOrder orderId = liftIO $ D.placeOrder conn orderId

    unplaceOrder :: Int -> Handler ()
    unplaceOrder = liftIO . (D.unplaceOrder conn)

    createHouseholdOrder :: Int -> Int -> Handler ()
    createHouseholdOrder householdId orderId = liftIO $ D.createHouseholdOrder conn householdId orderId

    deleteHouseholdOrder :: Int -> Int -> Handler ()
    deleteHouseholdOrder householdId orderId = liftIO $ D.deleteHouseholdOrder conn householdId orderId

    cancelHouseholdOrder :: Int -> Int -> Handler ()
    cancelHouseholdOrder householdId orderId = liftIO $ D.cancelHouseholdOrder conn householdId orderId

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder householdId orderId = liftIO $ D.completeHouseholdOrder conn householdId orderId

    reopenHouseholdOrder :: Int -> Int -> Handler ()
    reopenHouseholdOrder householdId orderId = liftIO $ D.reopenHouseholdOrder conn householdId orderId
 
    ensureHouseholdOrderItem :: Int -> Int -> Int -> HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem householdId orderId productId details = liftIO $ D.ensureHouseholdOrderItem conn householdId orderId productId details

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem householdId orderId productId = liftIO $ D.removeHouseholdOrderItem conn householdId orderId productId

    createHousehold :: HouseholdDetails -> Handler Int
    createHousehold details = liftIO $ D.createHousehold conn details

    updateHousehold :: Int -> HouseholdDetails -> Handler ()
    updateHousehold householdId details = liftIO $ D.updateHousehold conn householdId details

    archiveHousehold :: Int -> Handler ()
    archiveHousehold householdId = liftIO $ D.archiveHousehold conn householdId

    createProduct :: ProductDetails -> Handler Int
    createProduct details = liftIO $ D.createProduct conn details

    updateProduct :: Int -> ProductDetails -> Handler ()
    updateProduct productId details = liftIO $ D.updateProduct conn productId details

    archiveProduct :: Int -> Handler ()
    archiveProduct productId = liftIO $ D.archiveProduct conn productId
  
    createHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler Int
    createHouseholdPayment householdId details = liftIO $ D.createHouseholdPayment conn householdId details

    updateHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler ()
    updateHouseholdPayment householdPaymentId details = liftIO $ D.updateHouseholdPayment conn householdPaymentId details

    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment householdPaymentId = liftIO $ D.archiveHouseholdPayment conn householdPaymentId

    uploadProductList :: MultipartData -> Handler String
    uploadProductList multipartData = liftIO $ do
      let file = (files multipartData) !! 0
      copyFile (fdFilePath file) ("server/data/" ++ (unpack $ fdFileName file))
      return $ fdFilePath $ file