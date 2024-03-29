{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2.Domain.Adjustments where

import           Data.Time.Clock (UTCTime)
import           Data.List (find)
import qualified Data.Map as M (map, filter)
import           Data.Maybe (isJust, fromMaybe)
import           Prelude hiding (product)
import           Control.Lens

import V2.Domain.Types
import V2.Domain.Utils
import V2.Domain.Orders
import V2.Domain.Prices
import V2.Domain.Catalogue

orderAdjustment :: Order -> Maybe OrderAdjustment
orderAdjustment o = 
    if any (isJust . householdOrderAdjustment) . orderHouseholdOrdersToPlace $ o
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . map householdOrderAdjTotal . orderHouseholdOrdersToPlace $ o

orderIsReconciled :: Order -> Bool
orderIsReconciled = 
       (== OrderPlaced) . _orderStatus 
  .&&. all householdOrderIsReconciled . orderHouseholdOrdersToPlace

orderIsAwaitingCatalogueUpdateConfirm :: Order -> Bool
orderIsAwaitingCatalogueUpdateConfirm = 
    any householdOrderIsAwaitingCatalogueUpdateConfirm
  . orderHouseholdOrdersToPlace

applyCatalogueUpdate :: ProductCatalogue -> Order -> Order
applyCatalogueUpdate catalogue = 
    over (orderHouseholdOrdersWhere $ (/= HouseholdOrderAbandoned) . _householdOrderStatus) $
      applyUpdate catalogue

acceptCatalogueUpdate :: HouseholdId -> Order -> Order
acceptCatalogueUpdate householdId =
    over (orderHouseholdOrdersWhere $ hasHouseholdId householdId) $
      acceptUpdate

reconcileOrderItems :: UTCTime -> [(HouseholdId, OrderItemSpec)] -> Order -> Order
reconcileOrderItems date specs = 
    over (orderHouseholdOrders . traverse) $ 
      reconcile
  where
    reconcile ho = reconcileItems date (updatesForHousehold ho) ho
    updatesForHousehold ho = map snd . filter (((==) (householdOrderHouseholdId ho)) . fst) $ specs

householdOrderAdjTotal :: HouseholdOrder -> Money
householdOrderAdjTotal ho = 
  fromMaybe (householdOrderTotal ho) $ fmap _orderAdjNewTotal . householdOrderAdjustment $ ho

householdOrderAdjustment :: HouseholdOrder -> Maybe OrderAdjustment
householdOrderAdjustment ho =
    if any (isJust . _itemAdjustment) (_householdOrderItems ho)
      then Just $ OrderAdjustment adjTotal
      else Nothing
  where
    adjTotal = sum . M.map adjItemTotal . _householdOrderItems $ ho
    adjItemTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

householdOrderIsReconciled :: HouseholdOrder -> Bool
householdOrderIsReconciled = all (isJust . _itemAdjustment) . _householdOrderItems

householdOrderIsAwaitingCatalogueUpdateConfirm :: HouseholdOrder -> Bool
householdOrderIsAwaitingCatalogueUpdateConfirm = any (isJust . _itemAdjustment) .  _householdOrderItems

applyUpdate :: ProductCatalogue -> HouseholdOrder -> HouseholdOrder
applyUpdate catalogue = over (householdOrderItems . traverse) $ applyItemUpdate catalogue

acceptUpdate :: HouseholdOrder -> HouseholdOrder
acceptUpdate = over householdOrderItems $ 
      M.map clearAdjustment
    . deleteDiscontinuedItems
    . M.map acceptItemUpdate
  where
    clearAdjustment = itemAdjustment .~ Nothing
    deleteDiscontinuedItems = M.filter (not . fromMaybe False . fmap _itemAdjIsDiscontinued . _itemAdjustment)

reconcileItems :: UTCTime -> [OrderItemSpec] -> HouseholdOrder -> HouseholdOrder
reconcileItems date specs = over (householdOrderItems . traverse) reconcile
  where
    reconcile item = case find ((== itemProductCode item) . _itemSpecProductCode) specs of
      Just spec -> reconcileItem date spec item
      _ -> item
      
itemAdjTotal :: OrderItem -> Money
itemAdjTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

itemAdjNewTotal :: OrderItemAdjustment -> Money
itemAdjNewTotal adj = atQuantity (_itemAdjNewQuantity adj) (_itemAdjNewPrice adj)

adjustItemPrice :: UTCTime -> Price -> OrderItem -> OrderItem
adjustItemPrice date price i@OrderItem { _itemAdjustment = Just _ } = 
  i & itemAdjustment . _Just . itemAdjNewPrice .~ price
    & itemAdjustment . _Just . itemAdjDate .~ date
adjustItemPrice date price i = 
  i & itemAdjustment ?~ OrderItemAdjustment price (_itemQuantity i) False date

adjustItemQuantity :: UTCTime -> Int -> OrderItem -> OrderItem
adjustItemQuantity date quantity i@OrderItem { _itemAdjustment = Just _ } = 
  i & itemAdjustment . _Just . itemAdjNewQuantity .~ quantity
    & itemAdjustment . _Just . itemAdjDate .~ date
adjustItemQuantity date quantity i = 
  i & itemAdjustment ?~ OrderItemAdjustment (itemProductPrice i) quantity False date

-- WHY DOESN'T THIS WORK???
-- adjustItemPrice :: UTCTime -> Price -> OrderItem -> OrderItem
-- adjustItemPrice date price item = 
--     item & adjustmentOrDefault . itemAdjNewPrice .~ price
--          & adjustmentOrDefault . itemAdjDate .~ date
--   where
--     adjustmentOrDefault = itemAdjustment . non (OrderItemAdjustment price (_itemQuantity item) False date)

-- adjustItemQuantity :: UTCTime -> Int -> OrderItem -> OrderItem
-- adjustItemQuantity date quantity item = 
--     item & adjustmentOrDefault . itemAdjNewQuantity .~ quantity
--          & adjustmentOrDefault . itemAdjDate .~ date
--   where
--     adjustmentOrDefault = itemAdjustment . non (OrderItemAdjustment (itemProductPrice item) quantity False date)

discontinueProduct :: UTCTime -> OrderItem -> OrderItem
discontinueProduct date i@OrderItem { _itemAdjustment = Just _ } = 
  i & itemAdjustment . _Just . itemAdjNewQuantity .~ 0
    & itemAdjustment . _Just . itemAdjIsDiscontinued .~ True
    & itemAdjustment . _Just . itemAdjDate .~ date 
discontinueProduct date i =
  i & itemAdjustment ?~ OrderItemAdjustment (itemProductPrice i) 0 True date

applyItemUpdate :: ProductCatalogue -> OrderItem -> OrderItem
applyItemUpdate catalogue item = case findEntry (itemProductCode item) catalogue of
    Just e | itemProductPrice item == _catalogueEntryPrice e -> item
           | otherwise -> adjustItemPrice date (_catalogueEntryPrice e) item
    _ -> discontinueProduct date item
  where
    date = updatedDate catalogue

acceptItemUpdate :: OrderItem -> OrderItem
acceptItemUpdate item@OrderItem 
    { _itemAdjustment = Just (OrderItemAdjustment 
      { _itemAdjNewPrice = price
      , _itemAdjNewQuantity = quantity })
    } = 
  updateItemPrice price . updateItemQuantity (Just quantity) $ item
acceptItemUpdate item = item

reconcileItem :: UTCTime -> OrderItemSpec -> OrderItem -> OrderItem
reconcileItem date spec item = 
    adjustItemPrice date (reprice (_itemSpecProductPrice spec) (itemProductPrice item))
  . adjustItemQuantity date (_itemSpecQuantity spec) 
  $ item