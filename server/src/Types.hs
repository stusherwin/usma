{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Aeson.Types (Options(..))
import GHC.Generics
import Data.Char (toLower, isLower, toUpper)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import Prelude hiding (id)

dropFieldPrefixOptions = defaultOptions { fieldLabelModifier = dropFieldPrefix } where
  dropFieldPrefix = (first toLower) . (dropWhile isLower)
  first f (c:cs) = (f c):cs
  first _ [] = []

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

collectiveOrder :: Int -> UTCTime -> Maybe Int -> Maybe String -> Bool -> Int -> Int -> Int -> Int -> Bool -> [OrderItem] -> CollectiveOrder
collectiveOrder id createdDate createdBy createdByName complete _ _ totalExcVat totalIncVat True items = 
  CollectiveOrder id createdDate createdBy createdByName False False complete totalExcVat totalIncVat True Nothing items

collectiveOrder id createdDate createdBy createdByName complete oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat False items = 
  CollectiveOrder id createdDate createdBy createdByName False False complete totalExcVat totalIncVat False (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items

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
                                                                 
data HouseholdDetails = HouseholdDetails { hdetName :: String
                                         , hdetContactName :: Maybe String
                                         , hdetContactEmail :: Maybe String
                                         , hdetContactPhone :: Maybe String
                                         } deriving (Eq, Show, Generic)
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

data HouseholdOrderItemDetails = HouseholdOrderItemDetails { hoidetQuantity :: Maybe Int
                                                           } deriving (Eq, Show, Generic)
instance FromJSON HouseholdOrderItemDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

householdOrder :: Int -> UTCTime -> Maybe Int -> Maybe String -> Int -> String -> Bool -> Bool -> Int -> Int -> Int -> Int -> Bool -> [OrderItem] -> HouseholdOrder
householdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled _ _ totalExcVat totalIncVat False items = 
  HouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName False False householdId householdName complete cancelled open totalExcVat totalIncVat Nothing items 
  where
  open = not complete && not cancelled

householdOrder orderId orderCreated orderCreatedBy orderCreatedByName householdId householdName complete cancelled oldTotalExcVat oldTotalIncVat totalExcVat totalIncVat True items = 
  HouseholdOrder orderId orderCreated orderCreatedBy orderCreatedByName False False householdId householdName complete cancelled open totalExcVat totalIncVat (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items 
  where
  open = not complete && not cancelled

data HouseholdPayment = HouseholdPayment { hpId :: Int
                                         , hpHouseholdId :: Int
                                         , hpDate :: UTCTime
                                         , hpAmount :: Int
                                         } deriving (Eq, Show, Generic)
instance ToJSON HouseholdPayment where
  toJSON = genericToJSON dropFieldPrefixOptions

data HouseholdPaymentDetails = HouseholdPaymentDetails { hpdetDate :: Day
                                                       , hpdetAmount :: Int
                                                       } deriving (Eq, Show, Generic)
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

data OrderItemAdjustment = OrderItemAdjustment { oiaOldProductPriceExcVat :: Int
                                               , oiaOldProductPriceIncVat :: Int
                                               , oiaOldItemQuantity :: Int
                                               , oiaOldItemTotalExcVat :: Int
                                               , oiaOldItemTotalIncVat :: Int
                                               , oiaProductDiscontinued :: Bool
                                               } deriving (Eq, Show, Generic)
instance ToJSON OrderItemAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions

orderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan
  = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing

householdOrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan (Just oldProductPriceExcVat) (Just oldProductPriceIncVat) (Just oldItemQuantity) (Just oldItemTotalExcVat) (Just oldItemTotalIncVat) (Just productDiscontinued)
  = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan $ Just $ OrderItemAdjustment oldProductPriceExcVat oldProductPriceIncVat oldItemQuantity oldItemTotalExcVat oldItemTotalIncVat productDiscontinued

householdOrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan _ _ _ _ _ _
  = OrderItem productId productCode productName productVatRate productPriceExcVat productPriceIncVat itemQuantity itemTotalExcVat itemTotalIncVat biodynamic fairTrade glutenFree organic addedSugar vegan Nothing                     

data OrderAdjustment = OrderAdjustment { oaOldTotalExcVat :: Int
                                       , oaOldTotalIncVat :: Int 
                                       } deriving (Eq, Show, Generic)
instance ToJSON OrderAdjustment where
  toJSON = genericToJSON dropFieldPrefixOptions

data HouseholdQuantityDetails = HouseholdQuantityDetails { hqdetHouseholdId :: Int
                                                         , hqdetItemQuantity :: Int
                                                         } deriving (Eq, Show, Generic)
instance FromJSON HouseholdQuantityDetails where
  parseJSON = genericParseJSON dropFieldPrefixOptions

data ReconcileOrderItemDetails = ReconcileOrderItemDetails { roidetProductPriceExcVat :: Int
                                                           , roidetHouseholdQuantities :: [HouseholdQuantityDetails]
                                                           } deriving (Eq, Show, Generic)
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

pastCollectiveOrder :: Int -> UTCTime -> Maybe Int -> Maybe String -> Bool -> Bool -> Int -> Int -> Maybe Int -> Maybe Int -> [OrderItem] -> PastCollectiveOrder
pastCollectiveOrder id createdDate createdBy createdByName isAbandoned isReconciled totalExcVat totalIncVat (Just oldTotalExcVat) (Just oldTotalIncVat) items = 
  PastCollectiveOrder id createdDate createdBy createdByName (not isAbandoned) isAbandoned isAbandoned (not isAbandoned) isReconciled totalExcVat totalIncVat True (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items

pastCollectiveOrder id createdDate createdBy createdByName isAbandoned isReconciled totalExcVat totalIncVat _ _ items = 
  PastCollectiveOrder id createdDate createdBy createdByName (not isAbandoned) isAbandoned isAbandoned (not isAbandoned) isReconciled totalExcVat totalIncVat True Nothing items

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

pastHouseholdOrder :: Int -> UTCTime -> Maybe Int -> Maybe String -> Bool -> Int -> String -> Bool -> Bool -> Int -> Int -> Maybe Int -> Maybe Int -> [OrderItem] -> PastHouseholdOrder
pastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName isOrderAbandoned householdId householdName isAbandoned isReconciled totalExcVat totalIncVat (Just oldTotalExcVat) (Just oldTotalIncVat) items =
  PastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName (not isOrderAbandoned) isOrderAbandoned householdId householdName isAbandoned (not isAbandoned) False isReconciled totalExcVat totalIncVat (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) items

pastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName isOrderAbandoned householdId householdName isAbandoned isReconciled totalExcVat totalIncVat _ _ items =
  PastHouseholdOrder orderId orderCreatedDate orderCreatedBy orderCreatedByName (not isOrderAbandoned) isOrderAbandoned householdId householdName isAbandoned (not isAbandoned) False isReconciled totalExcVat totalIncVat Nothing items

data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
instance ToJSON VatRate

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

data GroupSettings = GroupSettings { gsEnablePayments :: Bool
                                   } deriving (Eq, Show, Generic)
instance ToJSON GroupSettings where
  toJSON = genericToJSON dropFieldPrefixOptions