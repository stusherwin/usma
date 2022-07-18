{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module V2.SumaCatalogue where

import           Control.Exception (SomeException(..), handle)  
import           Data.Aeson (Array, Object, Value, decode, (.:))
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict, readFile)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import qualified Data.Text as T (unpack, pack)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import           Data.Time.Calendar (addDays)
import           Data.Vector ((!?), (!))
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.TagSoup (parseTags, fromAttrib, fromTagText, sections, (~==))

import           V2.Repository (Repository, getProductData, setProductData, getVatRates, getProductCatalogueFile, setProductCatalogueFile)
import           V2.Domain (ProductCode(..), ProductCatalogue, parseCatalogue)

data ProductData = ProductData 
  { url :: String
  , title :: String
  , imageUrl :: String
  , size :: Int
  }

type FetchProductImage = Repository -> String -> IO (Maybe BL.ByteString)
type FetchProductImageFull = Repository -> String -> IO (Maybe BL.ByteString)

fetchProductImage :: FetchProductImage
fetchProductImage repo code = handle handleException $ do 
  productData <- getProductData repo (ProductCode code)
  putStrLn $ show productData
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

fetchProductImageFull :: FetchProductImageFull
fetchProductImageFull repo code = handle handleException $ do 
  productData <- getProductData repo (ProductCode code)
  productUrl <- case productData of
    Just (_, Just url, _, _, _) -> return $ Just url
    Just (Just i, _, _, Nothing, _) -> do
      productData <- fetchProductData code
      case productData of
        Just r -> do
          setProductData repo (ProductCode code) (Just i) (Just $ url r) (Just $ title r) (Just $ imageUrl r) (Just $ size r)
          return $ Just $ url r
        _ -> do
          return Nothing
    _ -> do
      productData <- fetchProductData code
      case productData of
        Just r -> do
          imageData <- simpleHttp (imageUrl r)
          setProductData repo (ProductCode code) (Just $ BL.toStrict imageData) (Just $ url r) (Just $ title r) (Just $ imageUrl r) (Just $ size r)
          return $ Just $ url r
        _ -> do
          return Nothing
  putStrLn $ show productUrl
  case productUrl of
    Just url -> do
      html <- simpleHttp url
      let scriptTags = sections (~== ("<script type=\"text/x-magento-init\">" :: String)) $ parseTags $ BL.unpack html
      let text = fromTagText $ scriptTags !! 13 !! 1
      let imageUrl = do
                      json <- decode $ BL.pack text
                      flip parseMaybe json $ \obj -> do
                                                      p <- obj .: "[data-gallery-role=gallery-placeholder]"
                                                      g <- p .: "mage/gallery/gallery"
                                                      d <- g .: "data"
                                                      let i = d ! 0
                                                      img :: String <- i .: "img"
                                                      return img
      case imageUrl of
        Just url -> do
          imageData <- simpleHttp url
          return $ Just $ imageData
        _ -> do
          img <- BL.readFile "client/static/img/404.jpg"
          return $ Just $ img
    _ -> do
      img <- BL.readFile "client/static/img/404.jpg"
      return $ Just $ img
  where
  handleException (SomeException ex) = do
    putStrLn $ show ex
    img <- BL.readFile "client/static/img/404.jpg"
    return $ Just $ img

fetchProductData :: String -> IO (Maybe ProductData)
fetchProductData code = do
  html <- simpleHttp ("http://www.sumawholesale.com/catalogsearch/result/?q=" ++ code)
  putStrLn $ BL.unpack html
  case sections (~== ("<div class=\"product details product-item-details\">" :: String)) $ parseTags $ BL.unpack html of
    (tags:_) -> do
      let a = tags !! 2
      putStrLn (show a)      
      let img = tags !! 8
      putStrLn (show img)      
      return $ Just $ ProductData { url = fromAttrib "href" a
                                  , title = fromAttrib "alt" img
                                  , imageUrl = fromAttrib "src" img
                                  , size = read $ fromAttrib "height" img
                                  }
    _ -> return Nothing

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
  