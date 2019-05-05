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
  import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
  import Control.Monad (mzero, when, void)
  
  import Api
  import qualified Database as D
  import CollectiveOrder
  import HouseholdOrder
  import PastCollectiveOrder
  import PastHouseholdOrder
  import Household
  import HouseholdPayment
  import Config
  import ProductCatalogueImport
  import ProductCatalogueEntry

  import Network.HTTP.Conduit
  import Text.HTML.TagSoup
  import qualified Data.ByteString.Lazy as L
  import qualified Data.ByteString.Lazy.Char8 as C
  import Data.Text.Encoding
  import System.IO (hFlush, stdout)
  import Control.Exception (handle, SomeException(..))

  data ProductData = ProductData { url :: String
                                 , title :: String
                                 , imageUrl :: String
                                 , size :: Int
                                 }

  fetchProductData :: String -> IO (Maybe ProductData)
  fetchProductData code = handle handleException $ do
    html <- simpleHttp ("https://www.sumawholesale.com/catalogsearch/result/?q=" ++ code)
    case sections (~== ("<p class=product-image>" :: String)) $ parseTags $ C.unpack html of
      [] -> return Nothing
      (tags:_) -> do
        let a = tags !! 2
        let img = tags !! 4
        return $ Just $ ProductData { url = fromAttrib "href" a
                                    , title = fromAttrib "title" a
                                    , imageUrl = fromAttrib "src" img
                                    , size = read $ fromAttrib "height" img
                                    }
    where
    handleException :: HttpException -> IO (Maybe ProductData)
    handleException _ = return Nothing

  fetchProductImage :: String -> IO (Maybe L.ByteString)
  fetchProductImage code = handle handleException $ do 
    let dir = "client/static/product-images/"
    let file = dir ++ code ++ ".jpg"
    exists <- doesFileExist file 
    when (not exists) $ do
      createDirectoryIfMissing True dir
      productData <- fetchProductData code
      case productData of
        Just r -> do
          imageData <- simpleHttp (imageUrl r)
          L.writeFile file imageData
        _ -> copyFile ("client/static/img/404.jpg") file
    image <- L.readFile file
    return $ Just image
    where
    handleException :: HttpException -> IO (Maybe L.ByteString)
    handleException _ = return Nothing
  
  main :: IO ()
  main = do
    config <- getConfig
    run (Config.port config) $ app config

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
  queryServer config = collectiveOrder
                  :<|> pastCollectiveOrders
                  :<|> householdOrders
                  :<|> pastHouseholdOrders
                  :<|> households
                  :<|> householdPayments
                  :<|> productCatalogue
                  :<|> productImage
    where
    conn = connectionString config
    
    collectiveOrder :: Handler (Maybe CollectiveOrder)
    collectiveOrder = liftIO $ D.getCollectiveOrder conn
    
    pastCollectiveOrders :: Handler [PastCollectiveOrder]
    pastCollectiveOrders = liftIO $ D.getPastCollectiveOrders conn
    
    householdOrders :: Handler [HouseholdOrder]
    householdOrders = liftIO $ D.getHouseholdOrders conn
    
    pastHouseholdOrders :: Handler [PastHouseholdOrder]
    pastHouseholdOrders = liftIO $ D.getPastHouseholdOrders conn

    households :: Handler [Household]
    households = liftIO $ D.getHouseholds conn

    householdPayments :: Handler [HouseholdPayment]
    householdPayments = liftIO $ D.getHouseholdPayments conn

    productCatalogue :: Handler [ProductCatalogueEntry]
    productCatalogue = liftIO $ D.getProductCatalogue conn

    productImage :: String -> Handler L.ByteString
    productImage code = do
      image <- liftIO $ fetchProductImage code
      case image of
        Just i -> return i
        _ -> throwError err404
      
  commandServer :: Config -> Server CommandAPI
  commandServer config = createOrder
                    :<|> deleteOrder
                    :<|> placeOrder
                    :<|> abandonOrder
                    :<|> createHouseholdOrder
                    :<|> deleteHouseholdOrder
                    :<|> abandonHouseholdOrder
                    :<|> completeHouseholdOrder
                    :<|> reopenHouseholdOrder
                    :<|> ensureHouseholdOrderItem
                    :<|> removeHouseholdOrderItem
                    :<|> createHousehold
                    :<|> updateHousehold
                    :<|> archiveHousehold
                    :<|> createHouseholdPayment
                    :<|> updateHouseholdPayment
                    :<|> archiveHouseholdPayment
                    :<|> uploadProductCatalogue
                    :<|> acceptCatalogueUpdates
    where
    conn = connectionString config
    
    createOrder :: Int -> Handler Int
    createOrder householdId = do
      date <- liftIO $ getCurrentTime
      liftIO $ D.createOrder conn date householdId

    deleteOrder :: Int -> Handler ()
    deleteOrder orderId = liftIO $ D.deleteOrder conn orderId

    placeOrder :: Int -> Handler ()
    placeOrder orderId = liftIO $ D.closeOrder conn False orderId

    abandonOrder :: Int -> Handler ()
    abandonOrder orderId = liftIO $ D.closeOrder conn True orderId

    createHouseholdOrder :: Int -> Int -> Handler ()
    createHouseholdOrder householdId orderId =  do
      date <- liftIO $ getCurrentTime
      liftIO $ D.createHouseholdOrder conn date householdId orderId

    deleteHouseholdOrder :: Int -> Int -> Handler ()
    deleteHouseholdOrder householdId orderId = liftIO $ D.deleteHouseholdOrder conn householdId orderId

    abandonHouseholdOrder :: Int -> Int -> Handler ()
    abandonHouseholdOrder householdId orderId = liftIO $ D.cancelHouseholdOrder conn householdId orderId

    completeHouseholdOrder :: Int -> Int -> Handler ()
    completeHouseholdOrder householdId orderId = liftIO $ D.completeHouseholdOrder conn householdId orderId

    reopenHouseholdOrder :: Int -> Int -> Handler ()
    reopenHouseholdOrder householdId orderId = liftIO $ D.reopenHouseholdOrder conn householdId orderId
 
    ensureHouseholdOrderItem :: Int -> Int -> String -> HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem householdId orderId productCode details = liftIO $ D.ensureHouseholdOrderItem conn householdId orderId productCode details

    removeHouseholdOrderItem :: Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem householdId orderId productId = liftIO $ D.removeHouseholdOrderItem conn householdId orderId productId

    createHousehold :: HouseholdDetails -> Handler Int
    createHousehold details = liftIO $ D.createHousehold conn details

    updateHousehold :: Int -> HouseholdDetails -> Handler ()
    updateHousehold householdId details = liftIO $ D.updateHousehold conn householdId details

    archiveHousehold :: Int -> Handler ()
    archiveHousehold householdId = liftIO $ D.archiveHousehold conn householdId

    createHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler Int
    createHouseholdPayment householdId details = liftIO $ D.createHouseholdPayment conn householdId details

    updateHouseholdPayment :: Int -> HouseholdPaymentDetails -> Handler ()
    updateHouseholdPayment householdPaymentId details = liftIO $ D.updateHouseholdPayment conn householdPaymentId details

    archiveHouseholdPayment :: Int -> Handler ()
    archiveHouseholdPayment householdPaymentId = liftIO $ D.archiveHouseholdPayment conn householdPaymentId

    uploadProductCatalogue :: MultipartData -> Handler ()
    uploadProductCatalogue multipartData = do
      when (length (files multipartData) /= 1) $
        throwError err400
      let file = (files multipartData) !! 0
      liftIO $ createDirectoryIfMissing True "server/data/uploads/"
      date <- liftIO $ getCurrentTime
      let day = utctDay date
      let destFilePath = "server/data/uploads/" ++ (formatTime defaultTimeLocale "%F" day) ++ "-" ++ (unpack $ fdFileName file)
      liftIO $copyFile (fdFilePath file) destFilePath
      liftIO $ importProductCatalogue conn date destFilePath

    acceptCatalogueUpdates :: Int -> Int -> Handler ()
    acceptCatalogueUpdates orderId householdId = do
      date <- liftIO $ getCurrentTime
      liftIO $ D.acceptCatalogueUpdates conn date orderId householdId