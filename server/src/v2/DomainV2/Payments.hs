{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Payments where

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

updatePayment :: UTCTime -> Int -> Payment -> Payment
updatePayment date amount p = p{ _paymentDate = date
                               , _paymentAmount = amount 
                               }
