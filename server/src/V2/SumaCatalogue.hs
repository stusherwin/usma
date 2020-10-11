module V2.SumaCatalogue where

import           Control.Exception (SomeException(..), handle)  
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict, readFile)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.TagSoup (parseTags, fromAttrib, sections, (~==))

--TODO: Extract catalogue stuff out of repository and domain
import           V2.Repository (Repository, getProductImage, setProductImage)
import           V2.Domain (ProductCode(..))

data WebsiteProductData = ProductData 
  { url :: String
  , title :: String
  , imageUrl :: String
  , size :: Int
  }

fetchDataFromWebsite :: String -> IO (Maybe WebsiteProductData)
fetchDataFromWebsite code = do
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

type FetchProductImage = Repository -> String -> IO (Maybe BL.ByteString)

fetchProductImage :: FetchProductImage
fetchProductImage repo code = handle handleException $ do 
  image <- getProductImage repo (ProductCode code)
  case image of
    Just i -> return $ Just $ BL.fromStrict i
    _ -> do
      productData <- fetchDataFromWebsite code
      case productData of
        Just r -> do
          imageData <- simpleHttp (imageUrl r)
          setProductImage repo (ProductCode code) $ BL.toStrict imageData
          return $ Just $ imageData
        _ -> do
          img <- BL.readFile "client/static/img/404.jpg"
          setProductImage repo (ProductCode code) $ BL.toStrict img
          return $ Just $ img
  where
  handleException (SomeException ex) = do
    putStrLn $ show ex
    img <- BL.readFile "client/static/img/404.jpg"
    return $ Just $ img
   