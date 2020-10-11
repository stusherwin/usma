{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Text (Text)
import Servant

import qualified V1.Api as V1 (Api)
import qualified V2.Api as V2 (Api)

type Api =
       "api" :> GroupApi
  :<|> "g" :> Capture "groupKey" Text :> Raw
  :<|> Raw

type GroupApi = 
  Capture "groupKey" Text :> (    VerifyApi
                             :<|> V2.Api 
                             :<|> V1.Api
                             )

type VerifyApi =
  "verify" :> Post '[JSON] Bool

fullApi :: Proxy Api
fullApi = Proxy