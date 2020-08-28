{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.HouseholdOrders where

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
import DomainV2.OrderItems
import DomainV2.Catalogue

whereHouseholdId :: HouseholdId -> HouseholdOrder -> Bool
whereHouseholdId householdId = (== householdId) . householdOrderHouseholdId

whereHousehold :: HouseholdInfo -> HouseholdOrder -> Bool
whereHousehold household = (== _householdId household) . householdOrderHouseholdId

newHouseholdOrder :: Order -> HouseholdInfo -> HouseholdOrder
newHouseholdOrder order household = HouseholdOrder (_orderInfo order) (_orderStatus order) household HouseholdOrderOpen []

householdOrderHouseholdId :: HouseholdOrder -> HouseholdId
householdOrderHouseholdId = _householdId . _householdOrderHouseholdInfo

householdOrderHouseholdName :: HouseholdOrder -> String
householdOrderHouseholdName = _householdName . _householdOrderHouseholdInfo

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . map itemTotal . _householdOrderItems

householdOrderAdjTotal :: HouseholdOrder -> Money
householdOrderAdjTotal ho = fromMaybe (householdOrderTotal ho) $ fmap _orderAdjNewTotal . householdOrderAdjustment $ ho

householdOrderAdjustment :: HouseholdOrder -> Maybe OrderAdjustment
householdOrderAdjustment ho =
    if any (isJust . _itemAdjustment) (_householdOrderItems ho)
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . map adjItemTotal . _householdOrderItems $ ho
    adjItemTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

householdOrderIsReconciled :: HouseholdOrder -> Bool
householdOrderIsReconciled = all (isJust . _itemAdjustment) . _householdOrderItems

householdOrderIsAwaitingCatalogueUpdateConfirm :: HouseholdOrder -> Bool
householdOrderIsAwaitingCatalogueUpdateConfirm = any (isJust . _itemAdjustment) .  _householdOrderItems

updateOrderStatus :: OrderStatus -> HouseholdOrder -> HouseholdOrder
updateOrderStatus status ho = ho { _householdOrderOrderStatus = status }

abandon :: HouseholdOrder -> HouseholdOrder
abandon ho = ho { _householdOrderStatus = HouseholdOrderAbandoned }

complete :: HouseholdOrder -> HouseholdOrder
complete ho = ho { _householdOrderStatus = HouseholdOrderComplete }

reopen :: HouseholdOrder -> HouseholdOrder
reopen ho = ho { _householdOrderStatus = HouseholdOrderOpen }

abandonIfNotComplete :: HouseholdOrder -> HouseholdOrder
abandonIfNotComplete ho | (== HouseholdOrderComplete) . _householdOrderStatus $ ho = ho 
                        | otherwise = abandon ho

addOrUpdateItems :: ProductCatalogue -> [(ProductCode, Maybe Int)] -> HouseholdOrder -> HouseholdOrder
addOrUpdateItems catalogue = over householdOrderItems
                           . addOrUpdate ((==) `on` itemProductCode) (updateItemQuantity . Just . _itemQuantity)
                           . catMaybes 
                           . map toOrderItem
  where
    toOrderItem (code, quantity) = findEntry code catalogue <&> \e ->
      OrderItem (fromCatalogueEntry e) (fromMaybe 1 quantity) Nothing

removeItem :: ProductCode -> HouseholdOrder -> HouseholdOrder
removeItem productCode = over householdOrderItems $ filter ((/= productCode) . itemProductCode)

applyUpdate :: ProductCatalogue -> HouseholdOrder -> HouseholdOrder
applyUpdate catalogue = over householdOrderItems $ map (applyItemUpdate catalogue)

acceptUpdate :: HouseholdOrder -> HouseholdOrder
acceptUpdate = over householdOrderItems $ map removeItemAdjustment
                                        . removeDiscontinued
                                        . map acceptItemUpdate
  where
    removeDiscontinued = filter (not . fromMaybe False . fmap _itemAdjIsDiscontinued . _itemAdjustment)

reconcileItems :: UTCTime -> [OrderItemSpec] -> HouseholdOrder -> HouseholdOrder
reconcileItems date specs = over (householdOrderItems . traverse) reconcile
  where
    reconcile item = case find ((== itemProductCode item) . _itemSpecProductCode) specs of
      Just spec -> reconcileItem date spec item
      _ -> item