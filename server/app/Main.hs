{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where
 
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Config
import App
import UpgradeDB
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  config <- getConfig "main.config"
  putStrLn "Upgrading database..."
  hFlush stdout
  upgradeDB config
  hFlush stdout
  putStrLn "Done."
  hFlush stdout
  setLocaleEncoding utf8
  run (Config.port config) $
    logStdoutDev $
    app config