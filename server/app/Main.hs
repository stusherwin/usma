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
import CompareV2Api (compareApiV1WithApiV2, recordApiV1Responses)
import UpgradeDB
import System.IO (hFlush, stdout)

import qualified V1_.ProductImage as V1_ (fetchProductImage)
import qualified V2_.SumaCatalogue as V2_ (fetchProductImage)

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
  record <- recordApiV1Responses "server/test/requests/"
  run (Config.port config) $
    logStdoutDev $
    compareApiV1WithApiV2 $
    record $ 
    app V1_.fetchProductImage V2_.fetchProductImage config