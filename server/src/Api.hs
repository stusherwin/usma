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
    :<|> "order-summary" :> Capture "orderId" Int :> Get '[JSON] OrderSummary
    :<|> "household-order-summary" :> Capture "orderId" Int :> Capture "householdId" Int :> Get '[JSON] HouseholdOrderSummary
 
  type CommandAPI =
         "create-order" :> Capture "date" Day :> Post '[JSON] Int
    :<|> "delete-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> ReqBody '[JSON] EnsureHouseholdOrderItem :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> ReqBody '[JSON] RemoveHouseholdOrderItem :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy