{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Api where
  import Data.Time.Calendar (Day)
  import Servant
  import Types
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment
 
  type AppAPI = 
         "api" :> (
              "query" :> QueryAPI
         :<|> "command" :> CommandAPI
         )

  type QueryAPI =
         "collective-orders" :> Get '[JSON] [CollectiveOrder]
    :<|> "household-orders" :> Get '[JSON] [HouseholdOrder]
    :<|> "products" :> Get '[JSON] [Product]
    :<|> "households" :> Get '[JSON] [Household]
    :<|> "household-payments" :> Get '[JSON] [HouseholdPayment]
 
  type CommandAPI =
         "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
    :<|> "create-order" :> Post '[JSON] Int
    :<|> "add-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "cancel-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "uncancel-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> ReqBody '[JSON] EnsureHouseholdOrderItem :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> ReqBody '[JSON] RemoveHouseholdOrderItem :> Post '[JSON] ()
    :<|> "create-household" :> ReqBody '[JSON] CreateHousehold :> Post '[JSON] Int
    :<|> "archive-household" :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "create-product" :> ReqBody '[JSON] CreateProduct :> Post '[JSON] Int
    :<|> "archive-product" :> Capture "productId" Int :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy