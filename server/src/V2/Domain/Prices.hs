{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2.Domain.Prices where

import           Control.Arrow ((&&&))
import           Data.Maybe (fromMaybe)
import           Prelude hiding (product)

import V2.Domain.Types

instance Num Money where
  Money exc1 inc1 + Money exc2 inc2 = Money (exc1 + exc2) (inc1 + inc2)
  Money exc1 inc1 - Money exc2 inc2 = Money (exc1 - exc2) (inc1 - inc2)
  Money exc1 inc1 * Money exc2 inc2 = Money (exc1 * exc2) (inc1 * inc2)
  abs    (Money exc inc) = Money (abs exc) (abs inc)
  signum (Money exc inc) = Money (signum exc) (signum inc)
  fromInteger i          = Money (fromInteger i) (fromInteger i)

atVatRate :: VatRate -> Int -> Price
atVatRate vatRate amountExcVat = Price vatRate (Money amountExcVat amountIncVat)
  where
    amountIncVat = round $ fromIntegral amountExcVat * (_vatRateMultiplier vatRate)

atQuantity :: Int -> Price -> Money
atQuantity quantity price = _priceAmount $ atVatRate vatRate (amountExcVat * fromIntegral quantity)
  where
    vatRate = _priceVatRate price
    amountExcVat = _moneyExcVat . _priceAmount $ price

reprice :: Int -> Price -> Price
reprice newAmountExcVat (Price vatRate _) = atVatRate vatRate newAmountExcVat

zeroRate :: VatRate
zeroRate = VatRate Zero 1

standardRate :: VatRate
standardRate = VatRate Standard 1.2

reducedRate :: VatRate
reducedRate = VatRate Reduced 1.05

findVatRate :: VatRateType -> [VatRate] -> VatRate
findVatRate t = fromMaybe zeroRate . lookup t . map (_vatRateType &&& id)