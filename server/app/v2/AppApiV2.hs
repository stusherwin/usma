{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AppApiV2 where

import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Char (toLower, isLower, toUpper)
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import           Network.HTTP.Media ((//))
import           Servant
import           Servant.Multipart (MultipartData, MultipartForm)
import qualified Types as Api

type AppApiV2 = 
  "v2" :> (    "query" :> QueryApiV2
          :<|> "command" :> CommandApiV2
          )

type QueryApiV2 =
       "data" :> Get '[JSON] Api.ApiData
  :<|> "product-catalogue-data" :> Get '[JSON] Api.ProductCatalogueApiData
  :<|> "collective-order" :> Get '[JSON] (Maybe Api.CollectiveOrder)
  :<|> "past-collective-orders" :> Get '[JSON] [Api.PastCollectiveOrder]
  :<|> "household-orders" :> Get '[JSON] [Api.HouseholdOrder]
  :<|> "past-household-orders" :> Get '[JSON] [Api.PastHouseholdOrder]
  :<|> "households" :> Get '[JSON] [Api.Household]
  :<|> "household-payments" :> Get '[JSON] [Api.HouseholdPayment]
  :<|> "product-catalogue" :> Get '[JSON] [Api.ProductCatalogueEntry]
  :<|> "product-imageApi." :> Capture "code" String :> Get '[Jpeg] BL.ByteString
  :<|> "collective-order-download" :> Get '[Csv] FileDownload
  :<|> "household-orders-download" :> Get '[Csv] FileDownload
  :<|> "past-collective-order-download" :> Capture "orderId" Int :> Get '[Csv] FileDownload
  :<|> "past-household-orders-download" :> Capture "orderId" Int :> Get '[Csv] FileDownload
  :<|> "product-catalogue-categories" :> Get '[JSON] [String]
  :<|> "product-catalogue-brands" :> Get '[JSON] [String]
  :<|> "group-settings" :> Get '[JSON] Api.GroupSettings

type CommandApiV2 =
       "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
  :<|> "create-order" :> Post '[JSON] Int
  :<|> "place-order"  :> Capture "orderId" Int :> Post '[JSON] ()
  :<|> "abandon-order" :> Capture "orderId" Int :> Post '[JSON] ()
  :<|> "abandon-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "complete-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reopen-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "ensure-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productCode" String :> ReqBody '[JSON] Api.HouseholdOrderItemDetails :> Post '[JSON] ()
  :<|> "ensure-all-past-order-items" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "pastOrderId" Int :> Post '[JSON] ()
  :<|> "remove-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productId" Int :> Post '[JSON] ()
  :<|> "create-household"  :> ReqBody '[JSON] Api.HouseholdDetails :> Post '[JSON] Int
  :<|> "update-household"  :> Capture "householdId" Int :> ReqBody '[JSON] Api.HouseholdDetails :> Post '[JSON] ()
  :<|> "archive-household" :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "create-household-payment" :> Capture "householdId" Int :> ReqBody '[JSON] Api.HouseholdPaymentDetails :> Post '[JSON] Int
  :<|> "update-household-payment" :> Capture "householdPaymentId" Int :> ReqBody '[JSON] Api.HouseholdPaymentDetails :> Post '[JSON] ()
  :<|> "archive-household-payment" :> Capture "householdPaymentId" Int :> Post '[JSON] ()
  :<|> "upload-product-catalogue" :> MultipartForm MultipartData :> Post '[JSON] ()
  :<|> "accept-catalogue-updates" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reconcile-order-item" :> Capture "orderId" Int :> Capture "productId" Int :> ReqBody '[JSON] Api.ReconcileOrderItemDetails :> Post '[JSON] ()
  :<|> "upload-order-file" :> MultipartForm MultipartData :> Post '[JSON] (Headers '[Header "Cache-Control" String] (Maybe Api.UploadedOrderFile))
  :<|> "reconcile-household-order-from-file" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "uuid" String :> Post '[JSON] ()

type FileDownload = Headers '[Header "Content-Disposition" Text] BL.ByteString

data Csv
instance Accept Csv where
  contentType _ = "text" // "csv"
instance MimeRender Csv (BL.ByteString) where
  mimeRender _ = Prelude.id

data Jpeg
instance Accept Jpeg where
  contentType _ = "image" // "jpeg"
instance MimeRender Jpeg (BL.ByteString) where
  mimeRender _ = Prelude.id