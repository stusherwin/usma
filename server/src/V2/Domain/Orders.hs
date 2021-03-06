{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2.Domain.Orders where

import           Control.Arrow ((&&&))
import qualified Data.Map.Strict as M (Map, delete, elems, empty, fromList, map, unionWith, unionsWith, adjust)
import           Data.Maybe (fromMaybe, catMaybes)
import           Control.Lens

import V2.Domain.Types
import V2.Domain.Utils
import V2.Domain.Catalogue
import V2.Domain.Prices

orderTotal :: Order -> Money
orderTotal = sum . M.map itemTotal . orderItemsToPlace

orderItemsToPlace :: Order -> M.Map ProductCode OrderItem
orderItemsToPlace = 
    M.unionsWith (<>)
  . map _householdOrderItems
  . orderHouseholdOrdersToPlace

orderHouseholdOrdersToPlace :: Order -> [HouseholdOrder]
orderHouseholdOrdersToPlace = 
    filter ((/= HouseholdOrderAbandoned) . _householdOrderStatus) 
  . _orderHouseholdOrders

orderIsComplete :: Order -> Bool
orderIsComplete = 
       (/= OrderAbandoned) . _orderStatus 
  .&&. (not . null) . orderHouseholdOrdersToPlace
  .&&. all ((== HouseholdOrderComplete) . _householdOrderStatus) . orderHouseholdOrdersToPlace

abandonOrder :: Order -> Order
abandonOrder = 
    (over (orderHouseholdOrders . traverse) $
      abandonIfNotComplete . set householdOrderOrderStatus OrderAbandoned)
  . set orderStatus OrderAbandoned
  where
    abandonIfNotComplete ho | (== HouseholdOrderComplete) . _householdOrderStatus $ ho = ho 
                            | otherwise = set householdOrderStatus HouseholdOrderAbandoned ho
                 
placeOrder :: Order -> Order
placeOrder = 
    (over (orderHouseholdOrders . traverse) $
      set householdOrderOrderStatus OrderPlaced)
  . set orderStatus OrderPlaced

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdId -> Order -> Order
abandonHouseholdOrder householdId = 
    updateOrderAbandonedStatus
  . (over (orderHouseholdOrdersWhere $ hasHouseholdId householdId) $ 
      set householdOrderStatus HouseholdOrderAbandoned)

reopenHouseholdOrder :: HouseholdId -> Order -> Order
reopenHouseholdOrder householdId = 
    updateOrderAbandonedStatus 
  . (over (orderHouseholdOrdersWhere $ hasHouseholdId householdId) $ 
      set householdOrderStatus HouseholdOrderOpen)

updateOrderAbandonedStatus :: Order -> Order
updateOrderAbandonedStatus o = 
    o & orderStatus .~ status
      & orderHouseholdOrders . traverse . householdOrderOrderStatus .~ status
  where 
    status = if all ((== HouseholdOrderAbandoned) . _householdOrderStatus) $ _orderHouseholdOrders o
               then OrderAbandoned
               else OrderOpen

completeHouseholdOrder :: HouseholdId -> Order -> Order
completeHouseholdOrder householdId = 
    over (orderHouseholdOrdersWhere $ hasHouseholdId householdId) $ 
      set householdOrderStatus HouseholdOrderComplete

addOrUpdateOrderItems :: ProductCatalogue -> HouseholdInfo -> [(ProductCode, Maybe Int)] -> Order -> Order
addOrUpdateOrderItems catalogue household itemQuantities =
    (over (orderHouseholdOrdersWhere $ hasHouseholdId $ _householdId household) $ 
      addOrUpdateHouseholdOrderItems catalogue itemQuantities)
  . ensureHouseholdOrder household

addOrderItemsFromPastOrder :: ProductCatalogue -> HouseholdInfo -> Order -> Order -> Order
addOrderItemsFromPastOrder catalogue household pastOrder = 
    (over (orderHouseholdOrdersWhere $ hasHouseholdId $ _householdId household) $ 
       addOrUpdateHouseholdOrderItems catalogue pastItemQuantities)
  . ensureHouseholdOrder household
  where
    pastItemQuantities = map (itemProductCode &&& Just . (const 1)) -- TODO: Should be . _itemQuantity)
                       . M.elems
                       . M.unionsWith (<>) 
                       . map _householdOrderItems
                       . filter (hasHouseholdId $ _householdId household)
                       . _orderHouseholdOrders 
                       $ pastOrder

ensureHouseholdOrder :: HouseholdInfo -> Order -> Order
ensureHouseholdOrder household o = 
    (over orderHouseholdOrders $ 
      ensure (hasHouseholdId $ _householdId household) $ newHouseholdOrder) o
  where
    newHouseholdOrder = HouseholdOrder (_orderInfo o) (_orderStatus o) household HouseholdOrderOpen M.empty

removeOrderItem :: HouseholdId -> ProductCode -> Order -> Order
removeOrderItem householdId productCode = 
    over (orderHouseholdOrdersWhere $ hasHouseholdId $ householdId) $ 
      over householdOrderItems $ M.delete productCode

updateOrderItem :: (OrderItem -> OrderItem) -> HouseholdId -> ProductCode -> Order -> Order
updateOrderItem f householdId productCode = 
    over (orderHouseholdOrdersWhere $ hasHouseholdId $ householdId) $ 
      over householdOrderItems $ M.adjust f productCode

hasHouseholdId :: HouseholdId -> HouseholdOrder -> Bool
hasHouseholdId householdId = (== householdId) . householdOrderHouseholdId

orderHouseholdOrdersWhere :: (HouseholdOrder -> Bool) -> (HouseholdOrder -> Identity HouseholdOrder) -> Order -> Identity Order
orderHouseholdOrdersWhere pred = orderHouseholdOrders . each . (filtered pred)

householdOrderHouseholdId :: HouseholdOrder -> HouseholdId
householdOrderHouseholdId = _householdId . _householdOrderHouseholdInfo

householdOrderHouseholdName :: HouseholdOrder -> String
householdOrderHouseholdName = _householdName . _householdOrderHouseholdInfo

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . M.map itemTotal . _householdOrderItems

addOrUpdateHouseholdOrderItems :: ProductCatalogue -> [(ProductCode, Maybe Int)] -> HouseholdOrder -> HouseholdOrder
addOrUpdateHouseholdOrderItems catalogue = 
      over householdOrderItems
    . M.unionWith (updateItemQuantity . Just . _itemQuantity)
    . M.fromList
    . map (itemProductCode &&& id)
    . catMaybes 
    . map toOrderItem
  where
    toOrderItem (code, quantity) = findEntry code catalogue <&> \e ->
      OrderItem (fromCatalogueEntry e) (fromMaybe 1 quantity) Nothing False

itemProductCode :: OrderItem -> ProductCode
itemProductCode = _productCode . _productInfo . _itemProduct

itemProductName :: OrderItem -> String
itemProductName = _productName . _productInfo . _itemProduct

itemProductPrice :: OrderItem -> Price
itemProductPrice = _productPrice . _productInfo . _itemProduct

itemTotal :: OrderItem -> Money
itemTotal item = atQuantity (_itemQuantity item) (itemProductPrice item)

updateItemPrice :: Price -> OrderItem -> OrderItem
updateItemPrice price = itemProduct . productInfo . productPrice .~ price

updateItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateItemQuantity quantity i = i & itemQuantity .~ fromMaybe (_itemQuantity i) quantity

toggleItemIsPacked :: OrderItem -> OrderItem
toggleItemIsPacked i = i & itemIsPacked %~ not