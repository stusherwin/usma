{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Payments where

import           Data.Time.Clock (UTCTime)
import           Control.Lens

import DomainV2.Types

updatePayment :: UTCTime -> Int -> Payment -> Payment
updatePayment date amount p = p & paymentDate .~ date
                                & paymentAmount .~ amount 