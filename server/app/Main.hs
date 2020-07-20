{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where
 
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Network.Wai.Handler.Warp (run)

import Config
import App

main :: IO ()
main = do
  config <- getConfig
  setLocaleEncoding utf8
  run (Config.port config) $ app config