{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Orders where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE (groupBy)
import           Prelude hiding (product)
import           Control.Lens

import DomainV2.Types
import DomainV2.Utils
import DomainV2.Catalogue
import DomainV2.Prices

orderTotal :: Order -> Money
orderTotal = sum . map itemTotal . orderItemsToPlace

orderItemsToPlace :: Order -> [OrderItem]
orderItemsToPlace = map sconcat  
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

abandonOrder :: Order -> Order
abandonOrder = over (orderHouseholdOrders . traverse) (abandonIfNotComplete . set householdOrderOrderStatus OrderAbandoned)
             . set orderStatus OrderAbandoned
  where
    abandonIfNotComplete ho | (== HouseholdOrderComplete) . _householdOrderStatus $ ho = ho 
                            | otherwise = set householdOrderStatus HouseholdOrderAbandoned ho
                 
placeOrder :: Order -> Order
placeOrder = over (orderHouseholdOrders . traverse) (set householdOrderOrderStatus OrderPlaced)
           . set orderStatus OrderPlaced

-- TODO: guard state eg. complete order can't be abandoned, placed order can't be abandoned etc
abandonHouseholdOrder :: HouseholdId -> Order -> Order
abandonHouseholdOrder householdId o = 
    over (orderHouseholdOrders . traverse) (set householdOrderOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . over orderHouseholdOrders (update (whereHouseholdId householdId) $ set householdOrderStatus HouseholdOrderAbandoned)
  $ o

reopenHouseholdOrder :: HouseholdId -> Order -> Order
reopenHouseholdOrder householdId o = 
    over (orderHouseholdOrders . traverse) (set householdOrderOrderStatus (_orderStatus o))
  . updateOrderAbandonedStatus 
  . over orderHouseholdOrders (update (whereHouseholdId householdId) $ set householdOrderStatus HouseholdOrderOpen)
  $ o

updateOrderAbandonedStatus :: Order -> Order
updateOrderAbandonedStatus o = o{ _orderStatus = if all ((== HouseholdOrderAbandoned) . _householdOrderStatus) $ _orderHouseholdOrders o
                                                   then OrderAbandoned
                                                   else OrderOpen
                                }

completeHouseholdOrder :: HouseholdId -> Order -> Order
completeHouseholdOrder householdId = 
    over orderHouseholdOrders $ update (whereHouseholdId householdId) (set householdOrderStatus HouseholdOrderComplete)

addOrUpdateOrderItems :: ProductCatalogue -> HouseholdInfo  -> [(ProductCode, Maybe Int)] -> Order -> Order
addOrUpdateOrderItems catalogue household itemQuantities =
    (over orderHouseholdOrders $ update (whereHousehold household) $ addOrUpdateHouseholdOrderItems catalogue itemQuantities)
  . ensureHouseholdOrder household

addOrderItemsFromPastOrder :: ProductCatalogue -> HouseholdInfo -> Order -> Order -> Order
addOrderItemsFromPastOrder catalogue household pastOrder = 
    (over orderHouseholdOrders $ update (whereHousehold household) $ addOrUpdateHouseholdOrderItems catalogue pastItemQuantities)
  . ensureHouseholdOrder household
  where
    pastItemQuantities = map (itemProductCode &&& Just . (const 1)) -- TODO: Should be . _itemQuantity)
                       . concatMap _householdOrderItems
                       . filter (whereHousehold household)
                       . _orderHouseholdOrders 
                       $ pastOrder

ensureHouseholdOrder :: HouseholdInfo -> Order -> Order
ensureHouseholdOrder household o = 
    (over orderHouseholdOrders $ ensure (whereHousehold household) $ newHouseholdOrder) o
  where
    newHouseholdOrder = HouseholdOrder (_orderInfo o) (_orderStatus o) household HouseholdOrderOpen []

removeOrderItem :: HouseholdId -> ProductCode -> Order -> Order
removeOrderItem householdId productCode = 
    over orderHouseholdOrders 
  $ update (whereHouseholdId householdId)
  $ over householdOrderItems $ filter ((/= productCode) . itemProductCode)

whereHouseholdId :: HouseholdId -> HouseholdOrder -> Bool
whereHouseholdId householdId = (== householdId) . householdOrderHouseholdId

whereHousehold :: HouseholdInfo -> HouseholdOrder -> Bool
whereHousehold household = (== _householdId household) . householdOrderHouseholdId

householdOrderHouseholdId :: HouseholdOrder -> HouseholdId
householdOrderHouseholdId = _householdId . _householdOrderHouseholdInfo

householdOrderHouseholdName :: HouseholdOrder -> String
householdOrderHouseholdName = _householdName . _householdOrderHouseholdInfo

householdOrderTotal :: HouseholdOrder -> Money
householdOrderTotal = sum . map itemTotal . _householdOrderItems

addOrUpdateHouseholdOrderItems :: ProductCatalogue -> [(ProductCode, Maybe Int)] -> HouseholdOrder -> HouseholdOrder
addOrUpdateHouseholdOrderItems catalogue = 
      over householdOrderItems
    . addOrUpdate ((==) `on` itemProductCode) (updateItemQuantity . Just . _itemQuantity)
    . catMaybes 
    . map toOrderItem
  where
    toOrderItem (code, quantity) = findEntry code catalogue <&> \e ->
      OrderItem (fromCatalogueEntry e) (fromMaybe 1 quantity) Nothing

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