{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module V1_.Types where

import Data.Aeson
import GHC.Generics
import Data.Char (toLower, isLower)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import Prelude hiding (id)

data CollectiveOrder = CollectiveOrder { coId :: Int
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
instance FromJSON CollectiveOrder where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data Household = Household { hId :: Int
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
instance FromJSON Household where
  parseJSON = genericParseJSON dropFieldPrefixOptions
                                                                 
data HouseholdDetails = HouseholdDetails { hdetName :: String
                                         , hdetContactName :: Maybe String
                                         , hdetContactEmail :: Maybe String
                                         , hdetContactPhone :: Maybe String
                                         } deriving (Eq, Show, Generic)
instance ToJSON HouseholdDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON HouseholdDetails where
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
instance FromJSON HouseholdOrder where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidetQuantity :: Maybe Int
                                                           } deriving (Eq, Show, Generic)
instance ToJSON HouseholdOrderItemDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON HouseholdOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdPayment = HouseholdPayment { hpId :: Int
                                         , hpHouseholdId :: Int
                                         , hpDate :: UTCTime
                                         , hpAmount :: Int
                                         } deriving (Eq, Show, Generic)
instance ToJSON HouseholdPayment where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON HouseholdPayment where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdPaymentDetails = HouseholdPaymentDetails { hpdetDate :: Day
                                                       , hpdetAmount :: Int
                                                       } deriving (Eq, Show, Generic)
instance ToJSON HouseholdPaymentDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON HouseholdPaymentDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data OrderItem = OrderItem { oiProductId :: Int
                           , oiProductCode :: String
                           , oiProductName :: String
                           , oiProductVatRate :: VatRate
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
instance FromJSON OrderItem where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data OrderItemAdjustment = OrderItemAdjustment { oiaOldProductPriceExcVat :: Int
                                               , oiaOldProductPriceIncVat :: Int
                                               , oiaOldItemQuantity :: Int
                                               , oiaOldItemTotalExcVat :: Int
                                               , oiaOldItemTotalIncVat :: Int
                                               , oiaProductDiscontinued :: Bool
                                               } deriving (Eq, Show, Generic)
instance ToJSON OrderItemAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON OrderItemAdjustment where
  parseJSON = genericParseJSON dropFieldPrefixOptions
 
data OrderAdjustment = OrderAdjustment { oaOldTotalExcVat :: Int
                                       , oaOldTotalIncVat :: Int 
                                       } deriving (Eq, Show, Generic)
instance ToJSON OrderAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON OrderAdjustment where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data HouseholdQuantityDetails = HouseholdQuantityDetails { hqdetHouseholdId :: Int
                                                         , hqdetItemQuantity :: Int
                                                         } deriving (Eq, Show, Generic)
instance ToJSON HouseholdQuantityDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON HouseholdQuantityDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data ReconcileOrderItemDetails = ReconcileOrderItemDetails { roidetProductPriceExcVat :: Int
                                                           , roidetHouseholdQuantities :: [HouseholdQuantityDetails]
                                                           } deriving (Eq, Show, Generic)
instance ToJSON ReconcileOrderItemDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON ReconcileOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data PastCollectiveOrder = PastCollectiveOrder { pcoId :: Int
                                               , pcoOrderCreatedDate :: UTCTime
                                               , pcoOrderCreatedBy :: Maybe Int
                                               , pcoOrderCreatedByName :: Maybe String
                                               , pcoOrderIsPlaced :: Bool
                                               , pcoOrderIsAbandoned :: Bool
                                               , pcoIsAbandoned :: Bool
                                               , pcoIsComplete :: Bool
                                               , pcoIsReconciled :: Bool
                                               , pcoTotalExcVat :: Int
                                               , pcoTotalIncVat :: Int
                                               , pcoAllHouseholdsUpToDate :: Bool
                                               , pcoAdjustment :: Maybe OrderAdjustment
                                               , pcoItems :: [OrderItem]
                                               } deriving (Eq, Show, Generic)
instance ToJSON PastCollectiveOrder where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON PastCollectiveOrder where
  parseJSON = genericParseJSON dropFieldPrefixOptions

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
instance FromJSON PastHouseholdOrder where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
instance ToJSON VatRate
instance FromJSON VatRate

data ProductCatalogueData = ProductCatalogueData { pcdCode :: String
                                                 , pcdCategory :: String
                                                 , pcdBrand :: String
                                                 , pcdDescription :: String
                                                 , pcdText :: String
                                                 , pcdSize :: String
                                                 , pcdPrice :: Int
                                                 , pcdVatRate :: VatRate
                                                 , pcdRrp :: Maybe Int
                                                 , pcdBiodynamic :: Bool
                                                 , pcdFairTrade :: Bool
                                                 , pcdGlutenFree :: Bool
                                                 , pcdOrganic :: Bool
                                                 , pcdAddedSugar :: Bool
                                                 , pcdVegan :: Bool
                                                 , pcdUpdated :: UTCTime
                                                 } deriving (Eq, Show, Generic)
instance ToJSON ProductCatalogueData where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON ProductCatalogueData where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data ProductCatalogueEntry = ProductCatalogueEntry { pceCode :: String
                                                   , pceName :: String
                                                   , pcePriceExcVat :: Int
                                                   , pcePriceIncVat :: Int
                                                   , pceVatRate :: VatRate
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
instance FromJSON ProductCatalogueEntry where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data GroupSettings = GroupSettings { gsEnablePayments :: Bool
                                   } deriving (Eq, Show, Generic)
instance ToJSON GroupSettings where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON GroupSettings where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data ReconcileHouseholdOrderItemDetails = ReconcileHouseholdOrderItemDetails { rhoidProductCode :: String
                                                                             , rhoidProductPriceExcVat :: Int
                                                                             , rhoidQuantity :: Int
                                                                             } deriving (Eq, Show, Generic)
instance ToJSON ReconcileHouseholdOrderItemDetails where
  toJSON = genericToJSON dropFieldPrefixOptions
instance FromJSON ReconcileHouseholdOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data UploadedOrderFile = UploadedOrderFile { fileId :: String 
                                           , orderDescription :: String
                                           , totalExcVat :: Int
                                           , totalIncVat :: Int
                                           , rows :: [UploadedOrderFileRow]
                                           } deriving (Eq, Show, Generic)
instance ToJSON UploadedOrderFile
instance FromJSON UploadedOrderFile

data UploadedOrderFileRow = UploadedOrderFileRow { code :: String 
                                                 , productDescription :: String
                                                 , productSize :: String
                                                 , price :: Int
                                                 , quantity :: Int
                                                 , total :: Int
                                                 } deriving (Eq, Show, Generic)
instance ToJSON UploadedOrderFileRow
instance FromJSON UploadedOrderFileRow

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
instance FromJSON ApiData

data ProductCatalogueApiData = ProductCatalogueApiData 
  { productCatalogue :: [ProductCatalogueEntry]
  , categories :: [String]
  , brands :: [String]
  } deriving (Eq, Show, Generic)
instance ToJSON ProductCatalogueApiData
instance FromJSON ProductCatalogueApiData

dropFieldPrefixOptions :: Options
dropFieldPrefixOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix } where
  dropFieldPrefix = (first toLower) . (dropWhile isLower)
  first f (c:cs) = (f c):cs
  first _ [] = []