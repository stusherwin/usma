{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where
  import System.Directory (getCurrentDirectory, listDirectory)
  import qualified Data.ByteString.Char8 as BC (unpack)
  import System.Command (readProcessWithExitCode, proc)
  import Control.Monad (forM_)
  import Data.List(sort)

  import Config

  upgradeDir = "/server/database/upgrade"

  main :: IO ()
  main = do
    config <- getConfig
    dir <- getCurrentDirectory
    scripts <- listDirectory $ dir  ++ upgradeDir
    forM_ (sort scripts) $ \s -> do
      putStrLn ""
      putStrLn $ "psql -a -f " ++ dir ++ upgradeDir ++ "/" ++ s ++ " " ++ (BC.unpack $ connectionString config)
      (exit, out, err) <- readProcessWithExitCode "psql" [
                              "-a", 
                              "-f", 
                              dir ++ upgradeDir ++ "/" ++ s, 
                              (BC.unpack $ connectionString config)
                            ] ""
      putStrLn $ show exit
