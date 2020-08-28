{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Orders where

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
import DomainV2.HouseholdOrders
import DomainV2.OrderItems
import DomainV2.Catalogue
import DomainV2.Prices

orderTotal :: Order -> Money
orderTotal = sum . map itemTotal . orderItems

orderAdjustment :: Order -> Maybe OrderAdjustment
orderAdjustment o = 
    if any (isJust . householdOrderAdjustment) $ orderHouseholdOrdersToPlace o
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . map householdOrderAdjTotal . orderHouseholdOrdersToPlace $ o

orderItems :: Order -> [OrderItem]
orderItems = map sconcat  
           . NE.groupBy ((==) `on` itemProductCode)
           . sortBy (compare `on` itemProductCode)
           . concatMap _householdOrderItems
           . orderHouseholdOrdersToPlace

orderHouseholdOrdersToPlace :: Order -> [HouseholdOrder]
orderHouseholdOrdersToPlace = filter ((/= HouseholdOrderAbandoned) . _householdOrderStatus) . _orderHouseholdOrders

orderIsComplete :: Order -> Bool
orderIsComplete = (/= OrderAbandoned) . _orderStatus 
             .&&. (not . null) . orderHouseholdOrdersToPlace
             .&&. all ((== HouseholdOrderComplete) . _householdOrderStatus) . orderHouseholdOrdersToPlace

orderIsReconciled :: Order -> Bool
orderIsReconciled = (== OrderPlaced) . _orderStatus 
               .&&. all householdOrderIsReconciled . orderHouseholdOrdersToPlace

orderIsAwaitingCatalogueUpdateConfirm :: Order -> Bool
orderIsAwaitingCatalogueUpdateConfirm = any householdOrderIsAwaitingCatalogueUpdateConfirm . orderHouseholdOrdersToPlace

abandonOrder :: Order -> Order
abandonOrder o = over orderHouseholdOrders (map $ abandonIfNotComplete . updateOrderStatus OrderAbandoned) o{ _orderStatus = OrderAbandoned }
                 
placeOrder :: Order -> Order
placeOrder o = over orderHouseholdOrders (map $ updateOrderStatus OrderPlaced) o{ _orderStatus = OrderPlaced }

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdId -> Order -> Order
abandonHouseholdOrder householdId o = 
    over orderHouseholdOrders (map $ updateOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . over orderHouseholdOrders (update (whereHouseholdId householdId) abandon)
  $ o

completeHouseholdOrder :: HouseholdId -> Order -> Order
completeHouseholdOrder householdId = 
    over orderHouseholdOrders $ update (whereHouseholdId householdId) complete

reopenHouseholdOrder :: ProductCatalogue -> HouseholdId -> Order -> Order
reopenHouseholdOrder catalogue householdId o = 
    over orderHouseholdOrders (map $ updateOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . over orderHouseholdOrders (update (whereHouseholdId householdId) $ applyUpdate catalogue . reopen)
  $ o

updateOrderAbandonedStatus :: Order -> Order
updateOrderAbandonedStatus o = o{ _orderStatus = if all ((== HouseholdOrderAbandoned) . _householdOrderStatus) $ _orderHouseholdOrders o
                                                   then OrderAbandoned
                                                   else OrderOpen
                                }

addOrUpdateHouseholdOrderItems :: ProductCatalogue -> HouseholdInfo  -> [(ProductCode, Maybe Int)] -> Order -> Order
addOrUpdateHouseholdOrderItems catalogue household itemQuantities =
    (over orderHouseholdOrders $ update (whereHousehold household) $ addOrUpdateItems catalogue itemQuantities)
  . ensureHouseholdOrder household

addItemsFromPastOrder :: ProductCatalogue -> HouseholdInfo -> Order -> Order -> Order
addItemsFromPastOrder catalogue household pastOrder = 
    (over orderHouseholdOrders $ update (whereHousehold household) $ addOrUpdateItems catalogue pastItemQuantities)
  . ensureHouseholdOrder household
  where
    pastItemQuantities = map (itemProductCode &&& Just . (const 1))-- TODO: Should be . _itemQuantity)
                       . concatMap _householdOrderItems
                       . filter (whereHousehold household)
                       . _orderHouseholdOrders 
                       $ pastOrder

ensureHouseholdOrder :: HouseholdInfo -> Order -> Order
ensureHouseholdOrder household o = 
    (over orderHouseholdOrders $ ensure (whereHousehold household) $ newHouseholdOrder o household) o

removeHouseholdOrderItem :: HouseholdId -> ProductCode -> Order -> Order
removeHouseholdOrderItem householdId productCode = 
    over orderHouseholdOrders 
  $ update (whereHouseholdId householdId)
  $ removeItem productCode

applyCatalogueUpdate :: ProductCatalogue -> Order -> Order
applyCatalogueUpdate catalogue = 
    over orderHouseholdOrders 
  $ update ((/= HouseholdOrderAbandoned) . _householdOrderStatus)
  $ applyUpdate catalogue

acceptCatalogueUpdate :: HouseholdId -> Order -> Order
acceptCatalogueUpdate householdId =
    over orderHouseholdOrders
  $ update (whereHouseholdId householdId)
  $ acceptUpdate

reconcileOrderItems :: UTCTime -> [(HouseholdId, OrderItemSpec)] -> Order -> Order
reconcileOrderItems date specs = 
    over orderHouseholdOrders (map reconcile)
  where
    reconcile ho = let updatesForHousehold = map snd . filter (((==) (householdOrderHouseholdId ho)) . fst) $ specs
                   in  reconcileItems date updatesForHousehold ho