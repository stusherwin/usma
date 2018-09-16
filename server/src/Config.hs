{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as B (pack, unpack)
  import System.Environment (getEnv, getArgs)

  data Config = Config { port :: Int
                       , connectionString :: ByteString
                       , productListDataFile :: String
                       }

  getConfig :: IO Config
  getConfig = do
    args <- getArgs
    let port = read $ head args
    putStrLn $ "port: " ++ (show port)
    connectionString <- if length args > 1 then return (args !! 1)
                                           else getEnv "DATABASE_URL"
    putStrLn $ "connection string: " ++ connectionString
    productListDataFile <- if length args > 2 then return (args !! 2)
                           else getEnv "PRODUCT_LIST_DATA_FILE"
    putStrLn $ "data file: " ++ productListDataFile
    return $ Config port (B.pack connectionString) productListDataFile