{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module CatalogueImport where
  import Data.Aeson
  import GHC.Generics
  import Config
  import Text.Read (readMaybe)
  import Control.Monad.IO.Class (liftIO)
  import Data.Text (Text)
  import qualified Data.Text as T (pack, unpack, strip)
  import System.Directory (getCurrentDirectory)
  import Data.Maybe (catMaybes, fromMaybe)
  import Data.Char (toLower)
  import Data.Time.Format (formatTime, defaultTimeLocale)
  import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
  import Data.ByteString (ByteString)
  import System.Directory (copyFile)
  import Product
  import CatalogueEntry
  import Database

  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn ch list = f list [[]] where
    f [] ws = map reverse $ reverse ws
    f (x:xs) ws | x == ch = f xs ([]:ws)
    f (x:xs) (w:ws) = f xs ((x:w):ws)

  loadCatalogue :: String -> IO [CatalogueEntry]
  loadCatalogue filePath = do 
    file <- readFile filePath
    return $ catMaybes $ zipWith parse [0..] $ map (splitOn ',') $ drop 1 $ lines file where
      parse i [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
        let -- desc' = unwords $ filter ((> 0) . length) $ map (T.unpack . T.strip . T.pack)
            --           [ brand
            --           , desc
            --           , if length size > 0 then "(" ++ map toLower size ++ ")" else ""
            --           , text
            --           ]
            price' = fromMaybe 0 $ (* 100) . round <$> (readMaybe price :: Maybe Float)
            vat' = case vat of
                     "1" -> Standard
                     "5" -> Reduced
                     _ -> Zero
            rrp' = (* 100) . round <$> (readMaybe price :: Maybe Float)
            b' = b == "B"
            f' = f == "F"
            g' = g == "G"
            o' = o == "O"
            s' = s == "S"
            v' = v == "V"
        in  Just $ CatalogueEntry code cat brand desc text size price' vat' rrp' b' f' g' o' s' v' Nothing
      parse _ _ = Nothing

  importCatalogue :: ByteString -> String -> IO ()
  importCatalogue connectionString filePath = do
    catalogue <- loadCatalogue filePath
    replaceCatalogue connectionString catalogue