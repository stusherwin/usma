{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Api where
  import Data.Aeson
  import GHC.Generics
  import Data.Time.Calendar (Day)
  import Servant
  import Servant.Multipart
  import CollectiveOrder
  import HouseholdOrder
  import Product
  import Household
  import HouseholdPayment
  import Data.Text (Text)
 
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
    :<|> "delete-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "place-order"  :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "unplace-order"  :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "create-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "delete-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "cancel-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "complete-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "reopen-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productId" Int :> ReqBody '[JSON] HouseholdOrderItemDetails :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productId" Int :> Post '[JSON] ()
    :<|> "create-household"  :> ReqBody '[JSON] HouseholdDetails :> Post '[JSON] Int
    :<|> "update-household"  :> Capture "householdId" Int :> ReqBody '[JSON] HouseholdDetails :> Post '[JSON] ()
    :<|> "archive-household" :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "create-product"  :> ReqBody '[JSON] ProductDetails :> Post '[JSON] Int
    :<|> "update-product"  :> Capture "productId" Int :> ReqBody '[JSON] ProductDetails :> Post '[JSON] ()
    :<|> "archive-product" :> Capture "productId" Int :> Post '[JSON] ()
    :<|> "create-household-payment" :> Capture "householdId" Int :> ReqBody '[JSON] HouseholdPaymentDetails :> Post '[JSON] Int
    :<|> "update-household-payment" :> Capture "householdPaymentId" Int :> ReqBody '[JSON] HouseholdPaymentDetails :> Post '[JSON] ()
    :<|> "archive-household-payment" :> Capture "householdPaymentId" Int :> Post '[JSON] ()
    :<|> "upload-product-catalogue" :> MultipartForm MultipartData :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy