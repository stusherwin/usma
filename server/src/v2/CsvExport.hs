{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module CsvExport where

import           Data.Csv (ToNamedRecord(..), (.=), namedRecord, encodeByName)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Vector as V (fromList)

import           DomainV2

data CsvRow = CsvRow
  { csvName :: String
  , csvCode :: String
  , csvPrice :: Int
  , csvQuantity :: Int
  , csvTotal :: Int
  , csvReference :: String
  }

instance ToNamedRecord CsvRow where
  toNamedRecord (CsvRow 
    { csvName = name
    , csvCode = code
    , csvPrice = price
    , csvQuantity = qty
    , csvTotal = tot
    , csvReference = ref
    }) 
    = namedRecord 
      [ "Product" .= name
      , "Code" .= code
      , "Price" .= price'
      , "Quantity" .= qty
      , "Total" .= total' 
      , "Reference" .= ref 
      ]
    where
    price' = ((fromIntegral price) :: Double) / 100.0
    total' = ((fromIntegral tot) :: Double) / 100.0

exportOrderItems :: Order -> BL.ByteString
exportOrderItems order = encodeByName columns rows
  where
    columns = V.fromList ["Code", "Product", "Price", "Quantity", "Total"]
    rows = map toRow $ orderItems order
    toRow i = CsvRow
      { csvName = itemProductName i
      , csvCode = fromProductCode . itemProductCode $ i
      , csvPrice = _moneyExcVat . _priceAmount . itemProductPrice $ i
      , csvQuantity = _itemQuantity i
      , csvTotal = _moneyExcVat . itemTotal $ i
      , csvReference = ""
      }

exportOrderItemsByHousehold :: Order -> BL.ByteString
exportOrderItemsByHousehold order = encodeByName columns rows
  where
    columns = V.fromList ["Code", "Product", "Price", "Quantity", "Total", "Reference"]
    rows = map toRow . concatMap (\ho -> map (householdOrderHouseholdName ho,) $ _householdOrderItems ho) . _orderHouseholdOrders $ order
    toRow (householdName, i) = CsvRow
      { csvName = itemProductName i
      , csvCode = fromProductCode . itemProductCode $ i
      , csvPrice = _moneyExcVat . _priceAmount . itemProductPrice $ i
      , csvQuantity = _itemQuantity i
      , csvTotal = _moneyExcVat . itemTotal $ i
      , csvReference = householdName
      }
