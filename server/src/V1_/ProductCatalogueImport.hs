{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module V1_.ProductCatalogueImport where
  import Text.Read (readMaybe)
  import Data.Maybe (catMaybes, fromMaybe)
  import Data.Time.Clock (UTCTime)
  import Data.ByteString (ByteString)

  import V1_.Types (ProductCatalogueData(..), VatRate(..))
  import V1_.Database (replaceProductCatalogue)

  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn ch list = f list [[]] where
    f _ [] = []
    f [] ws = map reverse $ reverse ws
    f (x:xs) ws | x == ch = f xs ([]:ws)
    f (x:xs) (w:ws) = f xs ((x:w):ws)

  loadProductCatalogue :: UTCTime -> String -> IO [ProductCatalogueData]
  loadProductCatalogue date fileContents = do 
    return $ catMaybes $ zipWith parse [0..] $ map (splitOn ',') $ drop 1 $ lines fileContents where
      parse _ [cat,brand,code,desc,text,size,price,vat,_,b,f,g,o,s,v,_] = 
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
  importProductCatalogue connectionString date fileContents = do
    catalogue <- loadProductCatalogue date fileContents
    replaceProductCatalogue connectionString date catalogue