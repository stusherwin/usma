{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Product where
  import Data.Aeson
  import GHC.Generics
  import Config
  import Text.Read (readMaybe)
  import Data.Text (Text)
  import qualified Data.Text as T (pack, unpack, strip)
  import System.Directory (getCurrentDirectory)
  import Data.Maybe (catMaybes, fromMaybe)
  import Data.Char (toLower)

  data Product = Product { id :: Int
                         , code :: String
                         , name :: String
                         , price :: Int
                         , vatRate :: VatRate
                         } deriving (Eq, Show, Generic)
  instance ToJSON Product

  data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
  instance ToJSON VatRate
  instance FromJSON VatRate

  splitOn :: Eq a => a -> [a] -> [[a]]
  splitOn ch list = f list [[]] where
    f [] ws = map reverse $ reverse ws
    f (x:xs) ws | x == ch = f xs ([]:ws)
    f (x:xs) (w:ws) = f xs ((x:w):ws)

  loadProductList :: Config -> IO [Product]
  loadProductList config = do 
    cwd <- getCurrentDirectory
    print cwd
    file <- readFile $ "server/data/" ++ (productListDataFile config)
    return $ take 100 $ catMaybes $ zipWith parse [0..] $ map (splitOn ',') $ drop 1 $ lines file where
      parse i [cat,brand,code,desc,text,size,price,vat,rrp,b,f,g,o,s,v,priceChange] = 
        let id = i+1
            desc' = unwords $ filter ((> 0) . length) $ map (T.unpack . T.strip . T.pack)
                      [ brand
                      , desc
                      , if length size > 0 then "(" ++ map toLower size ++ ")" else ""
                      , text
                      ]
            price' = 100 * (round $ fromMaybe 0.0 $ (readMaybe price :: Maybe Float))
            vat' = case vat of
                     "1" -> Standard
                     "5" -> Reduced
                     _ -> Zero
        in  Just $ Product id code desc' price' vat'
      parse _ _ = Nothing