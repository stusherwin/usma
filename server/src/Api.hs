{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Api where
  import Data.Text (Text)
  import Servant
  import Types
 
  type AppAPI = 
         "api" :> "orders" :> OrdersAPI
 
  type OrdersAPI =
         Get '[JSON] [Order] 
    
  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy