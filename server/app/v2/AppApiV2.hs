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
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Servant

type AppApiV2 = 
  "v2" :> ( "query" :> QueryApiV2
          )

type QueryApiV2 =
       "collective-order" :> Get '[JSON] (Maybe CollectiveOrder)

data CollectiveOrder = CollectiveOrder 
  { id :: Int
  , orderCreatedDate :: UTCTime
  , orderCreatedBy :: Maybe Int
  , orderCreatedByName :: Maybe String
  , orderIsPlaced :: Bool
  , orderIsAbandoned :: Bool
  , isComplete :: Bool
  , totalExcVat :: Int
  , totalIncVat :: Int
  , allHouseholdsUpToDate :: Bool
  , adjustment :: Maybe OrderAdjustment
  , items :: [OrderItem]
  } deriving (Eq, Show, Generic)
instance ToJSON CollectiveOrder

data OrderAdjustment = OrderAdjustment 
  { oldTotalExcVat :: Int
  , oldTotalIncVat :: Int 
  } deriving (Eq, Show, Generic)
instance ToJSON OrderAdjustment

data OrderItem = OrderItem 
  { productId :: Int
  , productCode :: String
  , productName :: String
  , productVatRate :: VatRate
  , productPriceExcVat :: Int
  , productPriceIncVat :: Int
  , itemQuantity :: Int
  , itemTotalExcVat :: Int
  , itemTotalIncVat :: Int
  , biodynamic :: Bool
  , fairTrade :: Bool
  , glutenFree :: Bool
  , organic :: Bool
  , addedSugar :: Bool
  , vegan :: Bool
  , adjustment :: Maybe OrderItemAdjustment
  } deriving (Eq, Show, Generic)
instance ToJSON OrderItem

data OrderItemAdjustment = OrderItemAdjustment 
  { oldProductPriceExcVat :: Int
  , oldProductPriceIncVat :: Int
  , oldItemQuantity :: Int
  , oldItemTotalExcVat :: Int
  , oldItemTotalIncVat :: Int
  , productDiscontinued :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON OrderItemAdjustment

data VatRate = Zero | Standard | Reduced deriving (Eq, Show, Generic)
instance ToJSON VatRate