module ProductImage where

import Text.HTML.TagSoup
import Control.Exception (SomeException(..), handle)
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict, readFile)
import Network.HTTP.Conduit

import qualified Database as D

data ProductData = ProductData { url :: String
                               , title :: String
                               , imageUrl :: String
                               , size :: Int
                               }

fetchProductData :: String -> IO (Maybe ProductData)
fetchProductData code = do
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

type FetchProductImage = B.ByteString -> String -> IO (Maybe BL.ByteString)

fetchProductImage :: FetchProductImage
fetchProductImage conn code = handle handleException $ do 
  image <- D.getProductImage conn code
  case image of
    Just i -> return $ Just $ BL.fromStrict i
    _ -> do
      productData <- fetchProductData code
      case productData of
        Just r -> do
          imageData <- simpleHttp (imageUrl r)
          D.saveProductImage conn code $ BL.toStrict imageData
          return $ Just $ imageData
        _ -> do
          img <- BL.readFile "client/static/img/404.jpg"
          D.saveProductImage conn code $ BL.toStrict img
          return $ Just $ img
  where
  handleException (SomeException ex) = do
    img <- BL.readFile "client/static/img/404.jpg"
    return $ Just $ img