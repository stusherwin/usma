module V2.SumaCatalogue where

import           Control.Exception (SomeException(..), handle)  
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict, readFile)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import qualified Data.Text as T (unpack, pack)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import           Data.Time.Calendar (addDays)
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.TagSoup (parseTags, fromAttrib, sections, (~==))

import           V2.Repository (Repository, getProductData, setProductData, getVatRates, getProductCatalogueFile, setProductCatalogueFile)
import           V2.Domain (ProductCode(..), ProductCatalogue, parseCatalogue)

data WebsiteProductData = ProductData 
  { url :: String
  , title :: String
  , imageUrl :: String
  , size :: Int
  }

type FetchProductImage = Repository -> String -> IO (Maybe BL.ByteString)

fetchProductImage :: FetchProductImage
fetchProductImage repo code = handle handleException $ do 
  productData <- getProductData repo (ProductCode code)
  case productData of
    Just (Just i, Just _, _, _, _) -> return $ Just $ BL.fromStrict i
    Just (Just i, Nothing, _, _, _) -> do
      productData <- fetchProductData code
      case productData of
        Just r -> do
          setProductData repo (ProductCode code) (Just i) (Just $ url r) (Just $ title r) (Just $ imageUrl r) (Just $ size r)
          return $ Just $ BL.fromStrict i
        _ -> do
          return $ Just $ BL.fromStrict i
    _ -> do
      productData <- fetchProductData code
      case productData of
        Just r -> do
          imageData <- simpleHttp (imageUrl r)
          setProductData repo (ProductCode code) (Just $ BL.toStrict imageData) (Just $ url r) (Just $ title r) (Just $ imageUrl r) (Just $ size r)
          return $ Just $ imageData
        _ -> do
          img <- BL.readFile "client/static/img/404.jpg"
          return $ Just $ img
  where
  handleException (SomeException ex) = do
    putStrLn $ show ex
    img <- BL.readFile "client/static/img/404.jpg"
    return $ Just $ img
   
fetchProductData :: String -> IO (Maybe WebsiteProductData)
fetchProductData code = do
  html <- simpleHttp ("https://www.sumawholesale.com/catalogsearch/result/?q=" ++ code)
  case sections (~== ("<p class=product-image>" :: String)) $ parseTags $ BL.unpack html of
    [] -> return Nothing
    (tags:_) -> do
      let a = tags !! 2
      let img = tags !! 4
      return $ Just $ ProductData { url = fromAttrib "href" a
                                  , title = fromAttrib "title" a
                                  , imageUrl = fromAttrib "src" img
                                  , size = read $ fromAttrib "height" img
                                  }

fetchProductCatalogue :: Repository -> IO (Maybe ProductCatalogue)
fetchProductCatalogue repo = handle handleException $ do
  today <- getCurrentTime
  let yesterday = UTCTime (addDays (-1) (utctDay today)) (utctDayTime today)

  maybeFile <- getProductCatalogueFile repo
  fileContents <- case maybeFile of
    -- Recently updated
    Just (updatedDate, fileContents) | diffUTCTime updatedDate yesterday > 0 -> do 
      return fileContents
    
    -- Needs updating
    _ -> do    
      fileContents <- T.pack . BL.unpack <$> simpleHttp ("https://www.suma.coop/downloads/CurrentSumaPrices.csv")
      setProductCatalogueFile repo today fileContents
      return fileContents

  vatRates <- getVatRates repo
  return $ Just $ parseCatalogue vatRates today $ T.unpack fileContents
  where
  handleException (SomeException ex) = do
    putStrLn $ show ex
    return $ Nothing
  