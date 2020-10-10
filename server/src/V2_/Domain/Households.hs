{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2_.Domain.Households where

import           Prelude hiding (product)
import           Control.Lens

import V2_.Domain.Types
import V2_.Domain.Utils
import V2_.Domain.Adjustments

householdTotalOrders :: Household -> Int
householdTotalOrders = _moneyIncVat
                     . (sum . map householdOrderAdjTotal)
                     . (filter $ (/= HouseholdOrderAbandoned) . _householdOrderStatus .&&. (/= OrderAbandoned) . _householdOrderOrderStatus)
                     . _householdOrders

householdTotalPayments :: Household -> Int
householdTotalPayments = (sum . map _paymentAmount)
                       . _householdPayments 

householdBalance :: Household -> Int
householdBalance h = householdTotalPayments h - householdTotalOrders h

updateHousehold :: String -> Contact -> Household -> Household
updateHousehold name contact h = h & householdInfo . householdName .~ name
                                   & householdContact .~ contact
