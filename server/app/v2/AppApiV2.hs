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
import Data.Time.Calendar (Day)
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
       "collective-order" :> Get '[JSON] (Maybe CollectiveOrder)
  :<|> "past-collective-orders" :> Get '[JSON] [CollectiveOrder]
  :<|> "household-orders" :> Get '[JSON] [HouseholdOrder]
  :<|> "past-household-orders" :> Get '[JSON] [PastHouseholdOrder]
  :<|> "households" :> Get '[JSON] [Household]
  :<|> "household-payments" :> Get '[JSON] [HouseholdPayment]
  :<|> "product-catalogue" :> Get '[JSON] [ProductCatalogueEntry]
  :<|> "product-catalogue-categories" :> Get '[JSON] [String]
  :<|> "product-catalogue-brands" :> Get '[JSON] [String]
  
type CommandApiV2 =
       "create-order" :> Capture "householdId" Int :> Post '[JSON] Int
  :<|> "create-order" :> Post '[JSON] Int
  :<|> "place-order"  :> Capture "orderId" Int :> Post '[JSON] ()
  :<|> "abandon-order" :> Capture "orderId" Int :> Post '[JSON] ()
  :<|> "abandon-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "complete-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
  :<|> "reopen-household-order" :> Capture "orderId" Int :> Capture "householdId" Int :> Post '[JSON] ()
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

data HouseholdPayment = HouseholdPayment { hpId :: Int
                                         , hpHouseholdId :: Int
                                         , hpDate :: UTCTime
                                         , hpAmount :: Int
                                         } deriving (Eq, Show, Generic)
instance ToJSON HouseholdPayment where
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

data ReconcileOrderItemDetails = ReconcileOrderItemDetails { roidetProductPriceExcVat :: Int
                                                           , roidetHouseholdQuantities :: [HouseholdQuantityDetails]
                                                           } deriving (Eq, Show, Generic)
instance FromJSON ReconcileOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdQuantityDetails = HouseholdQuantityDetails { hqdetHouseholdId :: Int
                                                         , hqdetItemQuantity :: Int
                                                         } deriving (Eq, Show, Generic)
instance FromJSON HouseholdQuantityDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdDetails = HouseholdDetails { hdetName :: String
                                         , hdetContactName :: Maybe String
                                         , hdetContactEmail :: Maybe String
                                         , hdetContactPhone :: Maybe String
                                         } deriving (Eq, Show, Generic)
instance FromJSON HouseholdDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdPaymentDetails = HouseholdPaymentDetails { hpdetDate :: Day
                                                       , hpdetAmount :: Int
                                                       } deriving (Eq, Show, Generic)
instance FromJSON HouseholdPaymentDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdOrder = HouseholdOrder { hoOrderId :: Int
                                     , hoOrderCreatedDate :: UTCTime
                                     , hoOrderCreatedBy :: Maybe Int
                                     , hoOrderCreatedByName :: Maybe String
                                     , hoOrderIsPlaced :: Bool
                                     , hoOrderIsAbandoned :: Bool
                                     , hoHouseholdId :: Int
                                     , hoHouseholdName :: String
                                     , hoIsComplete :: Bool
                                     , hoIsAbandoned :: Bool
                                     , hoIsOpen :: Bool
                                     , hoTotalExcVat :: Int
                                     , hoTotalIncVat :: Int
                                     , hoAdjustment :: Maybe OrderAdjustment
                                     , hoItems :: [OrderItem]
                                     } deriving (Eq, Show, Generic)
instance ToJSON HouseholdOrder where
  toJSON = genericToJSON dropFieldPrefixOptions

data PastHouseholdOrder = PastHouseholdOrder { phoOrderId :: Int
                                             , phoOrderCreatedDate :: UTCTime
                                             , phoOrderCreatedBy :: Maybe Int
                                             , phoOrderCreatedByName :: Maybe String
                                             , phoOrderIsPlaced :: Bool
                                             , phoOrderIsAbandoned :: Bool
                                             , phoHouseholdId :: Int
                                             , phoHouseholdName :: String 
                                             , phoIsAbandoned :: Bool
                                             , phoIsComplete :: Bool
                                             , phoIsOpen :: Bool
                                             , phoIsReconciled :: Bool
                                             , phoTotalExcVat :: Int
                                             , phoTotalIncVat :: Int
                                             , phoAdjustment :: Maybe OrderAdjustment
                                             , phoItems :: [OrderItem]
                                             } deriving (Eq, Show, Generic)
instance ToJSON PastHouseholdOrder where
  toJSON = genericToJSON dropFieldPrefixOptions

data ProductCatalogueEntry = ProductCatalogueEntry { pceCode :: String
                                                   , pceName :: String
                                                   , pcePriceExcVat :: Int
                                                   , pcePriceIncVat :: Int
                                                   , pceVatRate :: VatRateType
                                                   , pceBiodynamic :: Bool
                                                   , pceFairTrade :: Bool
                                                   , pceGlutenFree :: Bool
                                                   , pceOrganic :: Bool
                                                   , pceAddedSugar :: Bool
                                                   , pceVegan :: Bool
                                                   , pceCategory :: String
                                                   , pceBrand :: String
                                                   } deriving (Eq, Show, Generic)
instance ToJSON ProductCatalogueEntry where
  toJSON = genericToJSON dropFieldPrefixOptions

dropFieldPrefixOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix } where
  dropFieldPrefix = (first toLower) . (dropWhile isLower)
  first f (c:cs) = (f c):cs
  first _ [] = []