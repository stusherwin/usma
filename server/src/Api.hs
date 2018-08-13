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
    :<|> "products" :> Get '[JSON] [Product]
    :<|> "households" :> Get '[JSON] [Household]
    :<|> "order-summary" :> Capture "orderId" Int :> Get '[JSON] OrderSummary
    :<|> "household-order-summary" :> Capture "orderId" Int :> Capture "householdId" Int :> Get '[JSON] HouseholdOrderSummary
    :<|> "full-order-summary" :> Capture "orderId" Int :> Get '[JSON] FullOrderSummary
 
  type CommandAPI =
         "create-order" :> Capture "date" Day :> Post '[JSON] Int
    :<|> "delete-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "add-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "remove-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "cancel-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "uncancel-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> ReqBody '[JSON] EnsureHouseholdOrderItem :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> ReqBody '[JSON] RemoveHouseholdOrderItem :> Post '[JSON] ()
    :<|> "create-household" :> ReqBody '[JSON] CreateHousehold :> Post '[JSON] Int

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy