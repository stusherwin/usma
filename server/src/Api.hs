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

import qualified V1_.Api as V1_ (Api)
import qualified V2_.Api as V2_ (Api)

type Api =
       "api" :> GroupApi
  :<|> "g" :> Capture "groupKey" Text :> Raw
  :<|> Raw

type GroupApi = 
  Capture "groupKey" Text :> (    VerifyApi
                             :<|> V2_.Api 
                             :<|> V1_.Api
                             )

type VerifyApi =
  "verify" :> Post '[JSON] Bool

fullApi :: Proxy Api
fullApi = Proxy