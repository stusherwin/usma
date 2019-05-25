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
  import Data.Text (Text)
  import Data.Text as T (unpack)
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
  import Data.ByteString (ByteString)
  import Data.ByteString as B (unpack)

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
        _ -> return ()
    exists <- doesFileExist file 
    image <- L.readFile $ if exists then file else "client/static/img/404.jpg"
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
           
  -- server :: Config -> Server FullAPI
  -- server config = appServer config
  --            :<|> Tagged (staticPolicy (addBase "client/static") staticOrDefault)

  server :: Config -> Server FullAPI
  server config = appServer config
           :<|> serveFilesWithGroup
           :<|> serveFiles
           :<|> serveFiles
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

  appServer :: Config -> Server AppAPI
  appServer config = verifyServer config
                :<|> withGroupServer config

  verifyServer :: Config -> Server VerifyAPI
  verifyServer config groupKey = do
    group <- liftIO $ findGroup (connectionString config) groupKey
    case group of
      Just _ -> return True
      _ -> return False

  withGroupServer :: Config -> Server WithGroupAPI
  withGroupServer config groupKey = queryServer config groupKey
                             :<|> commandServer config groupKey

  queryServer :: Config -> Text -> Server QueryAPI
  queryServer config groupKey = collectiveOrder groupKey
                           :<|> pastCollectiveOrders groupKey
                           :<|> householdOrders groupKey
                           :<|> pastHouseholdOrders groupKey
                           :<|> households groupKey
                           :<|> householdPayments groupKey
                           :<|> productCatalogue
                           :<|> productImage
    where
    conn = connectionString config
    
    collectiveOrder :: Text -> Handler (Maybe CollectiveOrder)
    collectiveOrder groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getCollectiveOrder conn groupId
    
    pastCollectiveOrders :: Text -> Handler [PastCollectiveOrder]
    pastCollectiveOrders groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getPastCollectiveOrders conn groupId
    
    householdOrders :: Text -> Handler [HouseholdOrder]
    householdOrders groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getHouseholdOrders conn groupId
    
    pastHouseholdOrders :: Text -> Handler [PastHouseholdOrder]
    pastHouseholdOrders groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getPastHouseholdOrders conn groupId

    households :: Text -> Handler [Household]
    households groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getHouseholds conn groupId

    householdPayments :: Text -> Handler [HouseholdPayment]
    householdPayments groupKey = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.getHouseholdPayments conn groupId

    productCatalogue :: Handler [ProductCatalogueEntry]
    productCatalogue = liftIO $ D.getProductCatalogue conn

    productImage :: String -> Handler L.ByteString
    productImage code = do
      image <- liftIO $ fetchProductImage code
      case image of
        Just i -> return i
        _ -> throwError err404
      
  commandServer :: Config -> Text -> Server CommandAPI
  commandServer config groupKey = createOrder groupKey
                             :<|> deleteOrder groupKey
                             :<|> placeOrder groupKey
                             :<|> abandonOrder groupKey
                             :<|> createHouseholdOrder groupKey
                             :<|> deleteHouseholdOrder groupKey
                             :<|> abandonHouseholdOrder groupKey
                             :<|> completeHouseholdOrder groupKey
                             :<|> reopenHouseholdOrder groupKey
                             :<|> ensureHouseholdOrderItem groupKey
                             :<|> removeHouseholdOrderItem groupKey
                             :<|> createHousehold groupKey
                             :<|> updateHousehold groupKey
                             :<|> archiveHousehold groupKey
                             :<|> createHouseholdPayment groupKey
                             :<|> updateHouseholdPayment groupKey
                             :<|> archiveHouseholdPayment groupKey
                             :<|> uploadProductCatalogue
                             :<|> acceptCatalogueUpdates groupKey
    where
    conn = connectionString config
    
    createOrder :: Text -> Int -> Handler Int
    createOrder groupKey householdId = findGroupOr404 conn groupKey $ \groupId -> do
      date <- liftIO $ getCurrentTime
      liftIO $ D.createOrder conn groupId date householdId

    deleteOrder :: Text -> Int -> Handler ()
    deleteOrder groupKey orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.deleteOrder conn groupId orderId

    placeOrder :: Text -> Int -> Handler ()
    placeOrder groupKey orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.closeOrder conn groupId False orderId

    abandonOrder :: Text -> Int -> Handler ()
    abandonOrder groupKey orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.closeOrder conn groupId True orderId

    createHouseholdOrder :: Text -> Int -> Int -> Handler ()
    createHouseholdOrder groupKey householdId orderId = findGroupOr404 conn groupKey $ \groupId -> do
      date <- liftIO $ getCurrentTime
      liftIO $ D.createHouseholdOrder conn groupId date householdId orderId

    deleteHouseholdOrder :: Text -> Int -> Int -> Handler ()
    deleteHouseholdOrder groupKey householdId orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.deleteHouseholdOrder conn groupId householdId orderId

    abandonHouseholdOrder :: Text -> Int -> Int -> Handler ()
    abandonHouseholdOrder groupKey householdId orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.cancelHouseholdOrder conn groupId householdId orderId

    completeHouseholdOrder :: Text -> Int -> Int -> Handler ()
    completeHouseholdOrder groupKey householdId orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.completeHouseholdOrder conn groupId householdId orderId

    reopenHouseholdOrder :: Text -> Int -> Int -> Handler ()
    reopenHouseholdOrder groupKey householdId orderId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.reopenHouseholdOrder conn groupId householdId orderId
 
    ensureHouseholdOrderItem :: Text -> Int -> Int -> String -> HouseholdOrderItemDetails -> Handler ()
    ensureHouseholdOrderItem groupKey householdId orderId productCode details = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.ensureHouseholdOrderItem conn groupId householdId orderId productCode details

    removeHouseholdOrderItem :: Text -> Int -> Int -> Int -> Handler ()
    removeHouseholdOrderItem groupKey householdId orderId productId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.removeHouseholdOrderItem conn groupId householdId orderId productId

    createHousehold :: Text -> HouseholdDetails -> Handler Int
    createHousehold groupKey details = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.createHousehold conn groupId details

    updateHousehold :: Text -> Int -> HouseholdDetails -> Handler ()
    updateHousehold groupKey householdId details = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.updateHousehold conn groupId householdId details

    archiveHousehold :: Text -> Int -> Handler ()
    archiveHousehold groupKey householdId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.archiveHousehold conn groupId householdId

    createHouseholdPayment :: Text -> Int -> HouseholdPaymentDetails -> Handler Int
    createHouseholdPayment groupKey householdId details = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.createHouseholdPayment conn groupId householdId details

    updateHouseholdPayment :: Text -> Int -> HouseholdPaymentDetails -> Handler ()
    updateHouseholdPayment groupKey householdPaymentId details = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.updateHouseholdPayment conn groupId householdPaymentId details

    archiveHouseholdPayment :: Text -> Int -> Handler ()
    archiveHouseholdPayment groupKey householdPaymentId = findGroupOr404 conn groupKey $ \groupId ->
      liftIO $ D.archiveHouseholdPayment conn groupId householdPaymentId

    uploadProductCatalogue :: MultipartData -> Handler ()
    uploadProductCatalogue multipartData = do
      when (length (files multipartData) /= 1) $
        throwError err400
      let file = (files multipartData) !! 0
      liftIO $ createDirectoryIfMissing True "server/data/uploads/"
      date <- liftIO $ getCurrentTime
      let day = utctDay date
      let destFilePath = "server/data/uploads/" ++ (formatTime defaultTimeLocale "%F" day) ++ "-" ++ (T.unpack $ fdFileName file)
      liftIO $copyFile (fdFilePath file) destFilePath
      liftIO $ importProductCatalogue conn date destFilePath

    acceptCatalogueUpdates :: Text -> Int -> Int -> Handler ()
    acceptCatalogueUpdates groupKey orderId householdId = findGroupOr404 conn groupKey $ \groupId -> do
      date <- liftIO $ getCurrentTime
      liftIO $ D.acceptCatalogueUpdates conn groupId date orderId householdId

  findGroup :: ByteString -> Text -> IO (Maybe Int)
  findGroup conn groupKey = do
    rotaId <- D.getGroup conn (T.unpack groupKey)
    return rotaId

  findGroupOr404 :: ByteString -> Text -> (Int -> Handler a) -> Handler a
  findGroupOr404 conn groupKey handler = do
    groupId <- liftIO $ findGroup conn groupKey
    case groupId of
      Just id -> handler id
      _ -> throwError err404