{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Media ((//))

import Types
import AppApi (AppApi)
import AppApiV2 (AppApiV2)

type FullApi =
       "api" :> GroupApi
  :<|> "g" :> Capture "groupKey" Text :> Raw
  :<|> "g" :> Raw
  :<|> Raw

type GroupApi = 
  Capture "groupKey" Text :> (    VerifyApi
                             :<|> AppApiV2 
                             :<|> AppApi
                             )

type VerifyApi =
  "verify" :> Post '[JSON] Bool

fullApi :: Proxy FullApi
fullApi = Proxy