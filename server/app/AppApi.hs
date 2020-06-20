{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppApi where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Media ((//))

import Types
import UploadedOrderFile

data ApiData = ApiData 
  { collectiveOrder :: (Maybe CollectiveOrder)
  , pastCollectiveOrders :: [PastCollectiveOrder]
  , householdOrders :: [HouseholdOrder]
  , pastHouseholdOrders :: [PastHouseholdOrder]
  , households :: [Household]
  , householdPayments :: [HouseholdPayment]
  , groupSettings ::  GroupSettings
  } deriving (Eq, Show, Generic)
instance ToJSON ApiData

data ProductCatalogueApiData = ProductCatalogueApiData 
  { productCatalogue :: [ProductCatalogueEntry]
  , categories :: [String]
  , brands :: [String]
  } deriving (Eq, Show, Generic)
instance ToJSON ProductCatalogueApiData

type AppApi = 
       "query" :> QueryApi
  :<|> "command" :> CommandApi

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
  :<|> "product-image" :> Capture "code" String :> Get '[Jpeg] L.ByteString
  :<|> "collective-order-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
  :<|> "household-orders-download" :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
  :<|> "past-collective-order-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
  :<|> "past-household-orders-download" :> Capture "orderId" Int :> Get '[Csv] (Headers '[Header "Content-Disposition" Text] L.ByteString)
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
  :<|> "upload-product-catalogue" :> MultipartForm MultipartData :> Post '[JSON] ()
  :<|> "accept-catalogue-updates" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reconcile-order-item" :> Capture "orderId" Int :> Capture "productId" Int :> ReqBody '[JSON] ReconcileOrderItemDetails :> Post '[JSON] ()
  :<|> "upload-order-file" :> MultipartForm MultipartData :> Post '[JSON] (Headers '[Header "Cache-Control" String] (Maybe UploadedOrderFile))
  :<|> "reconcile-household-order-from-file" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "uuid" String :> Post '[JSON] ()

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