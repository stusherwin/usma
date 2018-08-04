{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Api where
  import Data.Time.Calendar (Day)
  import Servant
  import Types
 
  type AppAPI = 
         "api" :> (
              "query" :> QueryAPI
         :<|> "command" :> CommandAPI
         )

  type QueryAPI =
         "orders" :> Get '[JSON] [Order]
    :<|> "order-summary" :> Capture "orderId" Day :> Get '[JSON] OrderSummary
    :<|> "household-order-summary" :> Capture "orderId" Day :> Capture "householdId" Int :> Get '[JSON] HouseholdOrderSummary
 
  type CommandAPI =
         "create-order" :> Capture "date" Day :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy