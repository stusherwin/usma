{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as B (pack, unpack)
  import System.Environment (getEnv, getArgs)

  data Config = Config { port :: Int
                       , connectionString :: ByteString
                       }

  getConfig :: IO Config
  getConfig = do
    args <- getArgs
    let port = read $ head args
    putStrLn $ "port: " ++ (show port)
    connectionString <- if length args > 1 then return (args !! 1)
                                           else getEnv "DATABASE_URL"
    putStrLn $ "connection string: " ++ connectionString
    return $ Config port (B.pack connectionString)