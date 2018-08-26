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
    :<|> "archive-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "place-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "add-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "cancel-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "complete-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "reopen-household-order" :> ReqBody '[JSON] CancelHouseholdOrder :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> ReqBody '[JSON] EnsureHouseholdOrderItem :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> ReqBody '[JSON] RemoveHouseholdOrderItem :> Post '[JSON] ()
    :<|> "create-household" :> ReqBody '[JSON] CreateHousehold :> Post '[JSON] Int
    :<|> "update-household" :> ReqBody '[JSON] UpdateHousehold :> Post '[JSON] ()
    :<|> "archive-household" :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "create-product" :> ReqBody '[JSON] CreateProduct :> Post '[JSON] Int
    :<|> "update-product" :> ReqBody '[JSON] UpdateProduct :> Post '[JSON] ()
    :<|> "archive-product" :> Capture "productId" Int :> Post '[JSON] ()
    :<|> "create-household-payment" :> ReqBody '[JSON] CreateHouseholdPayment :> Post '[JSON] Int
    :<|> "update-household-payment" :> ReqBody '[JSON] UpdateHouseholdPayment :> Post '[JSON] ()
    :<|> "archive-household-payment" :> Capture "householdPaymentId" Int :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy