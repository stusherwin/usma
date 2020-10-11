{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2.Domain.Payments where

import           Data.Time.Clock (UTCTime)
import           Control.Lens

import V2.Domain.Types

updatePayment :: UTCTime -> Int -> Payment -> Payment
updatePayment date amount p = p & paymentDate .~ date
                                & paymentAmount .~ amount 