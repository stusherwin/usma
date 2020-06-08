{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AppApiV2 where

import Data.Aeson
import Data.Aeson.Types (Options(..))
import Data.Char (toLower, isLower, toUpper)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Servant
import Servant.Multipart (MultipartData, MultipartForm)

import DomainV2 (VatRateType)

type AppApiV2 = 
  "v2" :> (    "query" :> QueryApiV2
          :<|> "command" :> CommandApiV2
          )

type QueryApiV2 =
       "households" :> Get '[JSON] [Household]
  :<|> "collective-order" :> Get '[JSON] (Maybe CollectiveOrder)
  :<|> "past-collective-orders" :> Get '[JSON] [CollectiveOrder]

type CommandApiV2 =
       "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
  :<|> "create-order" :> Post '[JSON] Int
  :<|> "abandon-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "complete-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reopen-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "ensure-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productCode" String :> ReqBody '[JSON] HouseholdOrderItemDetails :> Post '[JSON] ()
  :<|> "remove-household-order-item" :> Capture "orderId" Int :> Capture "householdId" Int :> Capture "productId" Int :> Post '[JSON] ()
  :<|> "upload-product-catalogue" :> MultipartForm MultipartData :> Post '[JSON] ()

data Household = Household 
  { hId :: Int
  , hName :: String
  , hContactName :: Maybe String
  , hContactEmail :: Maybe String
  , hContactPhone :: Maybe String
  , hTotalOrders :: Int
  , hTotalPayments :: Int
  , hBalance :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Household where
  toJSON = genericToJSON dropFieldPrefixOptions

data CollectiveOrder = CollectiveOrder 
  { coId :: Int
  , coOrderCreatedDate :: UTCTime
  , coOrderCreatedBy :: Maybe Int
  , coOrderCreatedByName :: Maybe String
  , coOrderIsPlaced :: Bool
  , coOrderIsAbandoned :: Bool
  , coIsComplete :: Bool
  , coTotalExcVat :: Int
  , coTotalIncVat :: Int
  , coAllHouseholdsUpToDate :: Bool
  , coAdjustment :: Maybe OrderAdjustment
  , coItems :: [OrderItem]
  } deriving (Eq, Show, Generic)
instance ToJSON CollectiveOrder where
  toJSON = genericToJSON dropFieldPrefixOptions

data OrderAdjustment = OrderAdjustment 
  { oaOldTotalExcVat :: Int
  , oaOldTotalIncVat :: Int 
  } deriving (Eq, Show, Generic)
instance ToJSON OrderAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions

data OrderItem = OrderItem 
  { oiProductId :: Int
  , oiProductCode :: String
  , oiProductName :: String
  , oiProductVatRate :: VatRateType
  , oiProductPriceExcVat :: Int
  , oiProductPriceIncVat :: Int
  , oiItemQuantity :: Int
  , oiItemTotalExcVat :: Int
  , oiItemTotalIncVat :: Int
  , oiBiodynamic :: Bool
  , oiFairTrade :: Bool
  , oiGlutenFree :: Bool
  , oiOrganic :: Bool
  , oiAddedSugar :: Bool
  , oiVegan :: Bool
  , oiAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)
instance ToJSON OrderItem where
  toJSON = genericToJSON dropFieldPrefixOptions

data OrderItemAdjustment = OrderItemAdjustment 
  { oiaOldProductPriceExcVat :: Int
  , oiaOldProductPriceIncVat :: Int
  , oiaOldItemQuantity :: Int
  , oiaOldItemTotalExcVat :: Int
  , oiaOldItemTotalIncVat :: Int
  , oiaProductDiscontinued :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON OrderItemAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions

instance ToJSON VatRateType

data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidetQuantity :: Maybe Int
                                                           } deriving (Eq, Show, Generic)
instance FromJSON HouseholdOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

dropFieldPrefixOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix } where
  dropFieldPrefix = (first toLower) . (dropWhile isLower)
  first f (c:cs) = (f c):cs
  first _ [] = []