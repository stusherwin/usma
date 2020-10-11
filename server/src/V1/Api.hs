{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module V1.Api where

import Data.Text (Text)
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Media ((//))

import V1.Types

type Api = 
  "v1" :> (    "query" :> QueryApi
          :<|> "command" :> CommandApi
          )

type QueryApi =
       "data" :> Get '[JSON] ApiData
  :<|> "product-catalogue-data" :> Get '[JSON] ProductCatalogueApiData
  :<|> "collective-order" :> Get '[JSON] (Maybe CollectiveOrder)
  :<|> "past-collective-orders" :> Get '[JSON] [PastCollectiveOrder]
  :<|> "household-orders" :> Get '[JSON] [HouseholdOrder]
  :<|> "past-household-orders" :> Get '[JSON] [PastHouseholdOrder]
  :<|> "households" :> Get '[JSON] [Household]
  :<|> "household-payments" :> Get '[JSON] [HouseholdPayment]
  :<|> "product-catalogue" :> Get '[JSON] [ProductCatalogueEntry]
  :<|> "product-image" :> Capture "code" String :> Get '[Jpeg] BL.ByteString
  :<|> "collective-order-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  :<|> "household-orders-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  :<|> "past-collective-order-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  :<|> "past-household-orders-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] BL.ByteString)
  :<|> "product-catalogue-categories" :> Get '[JSON] [String]
  :<|> "product-catalogue-brands" :> Get '[JSON] [String]
  :<|> "group-settings" :> Get '[JSON] GroupSettings

type CommandApi =
       "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
  :<|> "create-order" :> Post '[JSON] Int
  :<|> "place-order"  :> Capture "orderId" Int :> Post '[JSON] ()
  :<|> "abandon-order"  :> Capture "orderId" Int :> Post '[JSON] ()
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
  :<|> "upload-product-catalogue" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] ()
  :<|> "accept-catalogue-updates" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reconcile-order-item" :> Capture "orderId" Int :> Capture "productId" Int :> ReqBody '[JSON] ReconcileOrderItemDetails :> Post '[JSON] ()
  :<|> "upload-order-file" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Headers '[Header "Cache-Control" String] (Maybe UploadedOrderFile))
  :<|> "reconcile-household-order-from-file" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "uuid" String :> Post '[JSON] ()

data Jpeg
instance Accept Jpeg where
  contentType _ = "image" // "jpeg"
instance MimeRender Jpeg (BL.ByteString) where
  mimeRender _ = Prelude.id
instance MimeUnrender Jpeg (BL.ByteString) where
  mimeUnrender _ = Prelude.id <$> Right

data Csv
instance Accept Csv where
  contentType _ = "text" // "csv"
instance MimeRender Csv (BL.ByteString) where
  mimeRender _ = Prelude.id
instance MimeUnrender Csv (BL.ByteString) where
  mimeUnrender _ = Prelude.id <$> Right
