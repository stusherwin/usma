{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Types where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.HashMap.Lazy as H (HashMap, fromList, lookup, elems)
import           Data.Hashable (Hashable)
import           Data.Time.Clock (UTCTime)
import           Data.List (maximumBy, find, partition, sortBy)
import           Data.List.Extra (trim, lower)
import           Data.Maybe (isJust, fromMaybe, catMaybes)
import           Data.Ord (comparing)
import           Data.Semigroup (sconcat)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE (groupBy, nonEmpty, toList)
import           GHC.Generics
import           Prelude hiding (product)
import           Text.Read (readMaybe)
import           Control.Lens (makeLenses)

import Debug.Trace(trace)

{- Household -}

data Household = Household 
  { _householdInfo :: HouseholdInfo
  , _householdContact :: Contact
  , _householdOrders :: [HouseholdOrder]
  , _householdPayments :: [Payment]
  } deriving (Eq, Show, Generic)

data Contact = Contact
  { _contactName :: Maybe String
  , _contactEmail :: Maybe String
  , _contactPhone :: Maybe String
  } deriving (Eq, Show, Generic)

data HouseholdSpec = HouseholdSpec
  { _householdSpecName :: String
  , _householdSpecContact :: Contact
  } deriving (Eq, Show, Generic)

newtype HouseholdId = HouseholdId 
  { fromHouseholdId :: Int 
  } deriving (Eq, Ord, Show, Generic)

data HouseholdInfo = HouseholdInfo 
  { _householdId :: HouseholdId
  , _householdName :: String
  } deriving (Eq, Show, Generic)

{- Payment -}

data Payment = Payment 
  { _paymentId :: PaymentId
  , _paymentHouseholdId :: HouseholdId
  , _paymentDate :: UTCTime
  , _paymentAmount :: Int
  } deriving (Eq, Show, Generic)

data PaymentSpec = PaymentSpec
  { _paymentSpecHouseholdId :: HouseholdId
  , _paymentSpecDate :: UTCTime
  , _paymentSpecAmount :: Int
  } deriving (Eq, Show, Generic)

newtype PaymentId = PaymentId 
  { fromPaymentId :: Int 
  } deriving (Eq, Ord, Show, Generic)

{-- OrderGroup --}

data OrderGroup = OrderGroup
  { _groupId :: OrderGroupId
  , _groupName :: String
  , _groupKey :: String
  , _groupSettings :: OrderGroupSettings
  }

data OrderGroupSettings = OrderGroupSettings
  { _groupSettingsPaymentsEnabled :: Bool
  }

newtype OrderGroupId = OrderGroupId 
  { fromOrderGroupId :: Int 
  } deriving (Eq, Ord, Show, Generic)

{-- Order --}

data OrderSpec = OrderSpec 
  { _orderSpecCreated :: UTCTime
  , _orderSpecCreatedByHouseholdId :: Maybe HouseholdId
  }

data Order = Order 
  { _orderInfo :: OrderInfo
  , _orderStatus :: OrderStatus
  , _orderHouseholdOrders :: [HouseholdOrder]
  } deriving (Eq, Show, Generic)

newtype OrderId = OrderId 
  { fromOrderId :: Int 
  } deriving (Eq, Ord, Show, Generic)

data OrderInfo = OrderInfo
  { _orderId :: OrderId
  , _orderGroupId :: OrderGroupId
  , _orderCreated :: UTCTime
  , _orderCreatedBy :: Maybe HouseholdInfo
  } deriving (Eq, Show, Generic)

data OrderStatus = OrderOpen
                 | OrderAbandoned
                 | OrderPlaced
                   deriving (Eq, Show, Generic)

data OrderAdjustment = OrderAdjustment 
  { _orderAdjNewTotal :: Money
  } deriving (Eq, Show, Generic)

{- HouseholdOrder -}

data HouseholdOrder = HouseholdOrder 
  { _householdOrderOrderInfo :: OrderInfo
  , _householdOrderOrderStatus :: OrderStatus
  , _householdOrderHouseholdInfo :: HouseholdInfo
  , _householdOrderStatus :: HouseholdOrderStatus
  , _householdOrderItems :: [OrderItem]
  } deriving (Eq, Show, Generic)

data HouseholdOrderStatus = HouseholdOrderOpen
                          | HouseholdOrderComplete
                          | HouseholdOrderAbandoned 
                          deriving (Eq, Show, Generic)

{- OrderItem -}

data OrderItem = OrderItem  
  { _itemProduct :: Product
  , _itemQuantity :: Int
  , _itemAdjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)

data OrderItemSpec = OrderItemSpec
  { _itemSpecProductCode :: ProductCode
  , _itemSpecProductPrice :: Int
  , _itemSpecQuantity :: Int
  } deriving (Eq, Show, Generic)

data OrderItemAdjustment = OrderItemAdjustment 
  { _itemAdjNewPrice :: Price
  , _itemAdjNewQuantity :: Int
  , _itemAdjIsDiscontinued :: Bool
  , _itemAdjDate :: UTCTime
  } deriving (Eq, Show, Generic)

{- Product -}

newtype ProductId = ProductId
  { fromProductId :: Int 
  } deriving (Eq, Ord, Show, Generic)

newtype ProductCode = ProductCode
  { fromProductCode :: String 
  } deriving (Eq, Ord, Show, Generic)

data ProductInfo = ProductInfo
  { _productCode :: ProductCode
  , _productName :: String
  , _productPrice :: Price
  } deriving (Eq, Show, Generic)

data Product = Product 
  { _productInfo :: ProductInfo
  , _productFlags :: ProductFlags
  } deriving (Eq, Show, Generic)

data ProductFlags = ProductFlags
  { _productIsBiodynamic :: Bool
  , _productIsFairTrade  :: Bool
  , _productIsGlutenFree :: Bool
  , _productIsOrganic    :: Bool
  , _productIsAddedSugar :: Bool
  , _productIsVegan      :: Bool
  } deriving (Eq, Show, Generic)

{- Price -}

data Price = Price
  { _priceVatRate :: VatRate
  , _priceAmount :: Money
  } deriving (Eq, Show, Generic)

{- Money -}

data Money = Money 
  { _moneyExcVat :: Int 
  , _moneyIncVat :: Int 
  } deriving (Eq, Ord, Show, Generic)

{- VatRate -}

data VatRateType = Zero 
                 | Standard 
                 | Reduced 
  deriving (Eq, Show, Generic)

data VatRate = VatRate
  { _vatRateType :: VatRateType
  , _vatRateMultiplier :: Rational
  } deriving (Eq, Show, Generic)

{- ProductCatalogue -}

data ProductCatalogueEntry = ProductCatalogueEntry
  { _catalogueEntryCode :: ProductCode
  , _catalogueEntryCategory :: String
  , _catalogueEntryBrand :: String
  , _catalogueEntryDescription :: String
  , _catalogueEntryText :: String
  , _catalogueEntrySize :: String
  , _catalogueEntryPrice :: Price
  , _catalogueEntryRrp :: Maybe Int
  , _catalogueEntryBiodynamic :: Bool
  , _catalogueEntryFairTrade :: Bool
  , _catalogueEntryGlutenFree :: Bool
  , _catalogueEntryOrganic :: Bool
  , _catalogueEntryAddedSugar :: Bool
  , _catalogueEntryVegan :: Bool
  , _catalogueEntryUpdated :: UTCTime
  } deriving (Eq, Show, Generic)

newtype ProductCatalogue = ProductCatalogue { fromProductCatalogue :: H.HashMap ProductCode ProductCatalogueEntry }

makeLenses ''Household
makeLenses ''Contact
makeLenses ''HouseholdSpec
makeLenses ''HouseholdInfo
makeLenses ''Payment
makeLenses ''PaymentSpec
makeLenses ''OrderGroup
makeLenses ''OrderGroupSettings
makeLenses ''OrderSpec
makeLenses ''Order
makeLenses ''OrderInfo
makeLenses ''OrderStatus
makeLenses ''OrderAdjustment
makeLenses ''HouseholdOrder
makeLenses ''HouseholdOrderStatus
makeLenses ''OrderItem
makeLenses ''OrderItemSpec
makeLenses ''OrderItemAdjustment
makeLenses ''ProductInfo
makeLenses ''Product
makeLenses ''ProductFlags
makeLenses ''Price
makeLenses ''Money
makeLenses ''VatRateType
makeLenses ''VatRate
makeLenses ''ProductCatalogueEntry