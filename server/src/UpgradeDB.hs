{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UpgradeDB where
  import System.Directory (getCurrentDirectory, listDirectory)
  import Data.ByteString (ByteString)
  import qualified Data.ByteString.Char8 as BC (unpack)
  import System.Process (readProcessWithExitCode)
  import Control.Monad (forM_)
  import Data.List(sort)

  import Config

  upgradeDir :: String
  upgradeDir = "/server/database/upgrade"

  upgradeDB :: Config -> IO ()
  upgradeDB config = do
    dir <- getCurrentDirectory
    scripts <- listDirectory $ dir ++ upgradeDir
    forM_ (sort scripts) $ \s -> do
      runScript (connectionString config) $ dir ++ upgradeDir ++ "/" ++ s

  runScript :: ByteString -> String -> IO ()
  runScript connectionString scriptFile = do
    putStrLn ""
    putStrLn $ "psql -a -f " ++ scriptFile ++ " " ++ (BC.unpack connectionString)
    (exit, _, err) <- readProcessWithExitCode "psql" [
                            "-a", 
                            "-f", 
                            scriptFile,
                            (BC.unpack connectionString)
                          ] ""
    putStrLn $ show exit
    putStrLn $ err
