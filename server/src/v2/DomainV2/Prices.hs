{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Prices where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import           Data.Functor ((<&>))
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
import           Control.Lens

import Debug.Trace(trace)

import DomainV2.Types
import DomainV2.Utils

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

getVatRate :: VatRateType -> [VatRate] -> VatRate
getVatRate t vs = fromMaybe zeroRate $ lookup t $ map (_vatRateType &&& id) $ vs