{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Households where

import           Prelude hiding (product)

import DomainV2.Types
import DomainV2.Utils
import DomainV2.Adjustments

householdTotalOrders :: Household -> Int
householdTotalOrders = _moneyIncVat
                     . (sum . map householdOrderAdjTotal)
                     . filter ((/= HouseholdOrderAbandoned) . _householdOrderStatus .&&. (/= OrderAbandoned) . _householdOrderOrderStatus)
                     . _householdOrders

householdTotalPayments :: Household -> Int
householdTotalPayments = (sum . map _paymentAmount)
                       . _householdPayments 

householdBalance :: Household -> Int
householdBalance h = householdTotalPayments h - householdTotalOrders h

updateHousehold :: String -> Contact -> Household -> Household
updateHousehold name contact h = let i = _householdInfo h
                                 in  h{ _householdInfo = i{ _householdName = name }
                                      , _householdContact = contact 
                                      }
