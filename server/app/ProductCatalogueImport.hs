{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ProductCatalogueImport where
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
  import Data.Time.Calendar (Day)
  import Data.Time.Format (formatTime, defaultTimeLocale)
  import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
  import Data.ByteString (ByteString)
  import System.Directory (copyFile)

  import Types (ProductCatalogueData(..), VatRate(..))
  import Database (replaceProductCatalogue)

  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn ch list = f list [[]] where
    f [] ws = map reverse $ reverse ws
    f (x:xs) ws | x == ch = f xs ([]:ws)
    f (x:xs) (w:ws) = f xs ((x:w):ws)

  loadProductCatalogue :: UTCTime -> String -> IO [ProductCatalogueData]
  loadProductCatalogue date filePath = do 
    file <- readFile filePath
    return $ catMaybes $ zipWith parse [0..] $ map (splitOn ',') $ drop 1 $ lines file where
      parse i [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
        let price' = fromMaybe 0 $ round . (* 100) <$> (readMaybe price :: Maybe Float)
            vat' = case vat of
                     "1" -> Standard
                     "5" -> Reduced
                     _ -> Zero
            rrp' =  round . (* 100) <$> (readMaybe price :: Maybe Float)
            b' = b == "B"
            f' = f == "F"
            g' = g == "G"
            o' = o == "O"
            s' = s == "S"
            v' = v == "V"
        in  Just $ ProductCatalogueData code cat brand desc text size price' vat' rrp' b' f' g' o' s' v' date
      parse _ _ = Nothing

  importProductCatalogue :: ByteString -> UTCTime -> String -> IO ()
  importProductCatalogue connectionString date filePath = do
    catalogue <- loadProductCatalogue date filePath
    replaceProductCatalogue connectionString date catalogue