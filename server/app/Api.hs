{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where
  import Data.Aeson
  import GHC.Generics
  import Data.Text (Text)
  import Data.Time.Calendar (Day)
  import Servant
  import Servant.Multipart
  import CollectiveOrder
  import HouseholdOrder
  import PastCollectiveOrder
  import PastHouseholdOrder
  import Household
  import HouseholdPayment
  import OrderItem
  import ProductCatalogueEntry
  import qualified Data.ByteString.Lazy as L
  import Network.HTTP.Media ((//))

  data Jpeg

  instance Accept Jpeg where
    contentType _ = "image" // "jpeg"

  instance MimeRender Jpeg (L.ByteString) where
    mimeRender _ = Prelude.id
 
  data Csv

  instance Accept Csv where
    contentType _ = "text" // "csv"

  instance MimeRender Csv (L.ByteString) where
    mimeRender _ = Prelude.id

  type AppAPI = 
    "api" :> (
           "verify" :> VerifyAPI
      :<|> WithGroupAPI
    )

  type VerifyAPI =
         Capture "groupKey" Text :> Post '[JSON] Bool

  type WithGroupAPI =
    Capture "rotaKey" Text :> (
              "query" :> QueryAPI
         :<|> "command" :> CommandAPI
    )

  type QueryAPI =
         "collective-order" :> Get '[JSON] (Maybe CollectiveOrder)
    :<|> "past-collective-orders" :> Get '[JSON] [PastCollectiveOrder]
    :<|> "household-orders" :> Get '[JSON] [HouseholdOrder]
    :<|> "past-household-orders" :> Get '[JSON] [PastHouseholdOrder]
    :<|> "households" :> Get '[JSON] [Household]
    :<|> "household-payments" :> Get '[JSON] [HouseholdPayment]
    :<|> "product-catalogue" :> Get '[JSON] [ProductCatalogueEntry]
    :<|> "product-image" :> Capture "code" String :> Get '[Jpeg] L.ByteString
    :<|> "collective-order-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
    :<|> "household-orders-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
    :<|> "past-collective-order-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
    :<|> "past-household-orders-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
    :<|> "product-catalogue-categories" :> Get '[JSON] [String]
    :<|> "product-catalogue-brands" :> Get '[JSON] [String]
 
  type CommandAPI =
         "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
    :<|> "delete-order" :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "place-order"  :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "abandon-order"  :> Capture "orderId" Int :> Post '[JSON] ()
    :<|> "create-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "delete-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "abandon-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "complete-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "reopen-household-order"   :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "ensure-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productCode" String :> ReqBody '[JSON] HouseholdOrderItemDetails :> Post '[JSON] ()
    :<|> "ensure-all-past-order-items" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "pastOrderId" Int :> Post '[JSON] ()
    :<|> "remove-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productId" Int :> Post '[JSON] ()
    :<|> "create-household"  :> ReqBody '[JSON] HouseholdDetails :> Post '[JSON] Int
    :<|> "update-household"  :> Capture "householdId" Int :> ReqBody '[JSON] HouseholdDetails :> Post '[JSON] ()
    :<|> "archive-household" :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "create-household-payment" :> Capture "householdId" Int :> ReqBody '[JSON] HouseholdPaymentDetails :> Post '[JSON] Int
    :<|> "update-household-payment" :> Capture "householdPaymentId" Int :> ReqBody '[JSON] HouseholdPaymentDetails :> Post '[JSON] ()
    :<|> "archive-household-payment" :> Capture "householdPaymentId" Int :> Post '[JSON] ()
    :<|> "upload-product-catalogue" :> MultipartForm MultipartData :> Post '[JSON] ()
    :<|> "accept-catalogue-updates" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
    :<|> "reconcile-order-item" :> Capture "orderId" Int :> Capture "productId" Int :> ReqBody '[JSON] ReconcileOrderItemDetails :> Post '[JSON] ()

  type FullAPI =
         AppAPI
    :<|> "g" :> Capture "groupKey" Text :> Raw
    :<|> "g" :> Raw
    :<|> Raw
  
  fullAPI :: Proxy FullAPI
  fullAPI = Proxy

  appAPI :: Proxy AppAPI
  appAPI = Proxy