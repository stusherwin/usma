{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config where
  import           Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as B (pack)
  import           Control.Exception (SomeException(..), catch)
  import           System.Environment (getEnv, getArgs)

  data Config = Config { port :: Int
                       , connectionString :: ByteString
                       , connectionStringV2 :: ByteString
                       } deriving (Read, Show)

  getConfig :: String -> IO Config
  getConfig configFile = catch (read <$> readFile configFile) $ \(SomeException _) -> do
    args <- getArgs
    let port = case args of
                (a:_) -> read $ a
                _     -> 0
    putStrLn $ "port: " ++ (show port)
    connectionString <- case args of
                          (_:cs:_) -> return cs
                          _        -> getEnv "DATABASE_URL"
    putStrLn $ "connection string: " ++ connectionString
    connectionStringV2 <- case args of
                          (_:_:cs:_) -> return cs
                          _          -> getEnv "DATABASE_URL"
    return $ Config port (B.pack connectionString) (B.pack connectionStringV2)