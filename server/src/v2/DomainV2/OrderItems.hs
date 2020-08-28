{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.OrderItems where

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
import DomainV2.Catalogue
import DomainV2.Prices
import DomainV2.Adjustments

instance Semigroup OrderItem where
  i1 <> i2 = OrderItem p (q1 + q2) (a1 <> a2)
    where
      p        = product (_itemProduct i1) (_itemAdjustment i1) (_itemProduct i2) (_itemAdjustment i2)
      (a1, a2) = adjustments (_itemAdjustment i1) (_itemAdjustment i2)
      (q1, q2) = (_itemQuantity i1, _itemQuantity i2)

      product _  Nothing p2 (Just _) = p2
      product p1 _       _  _        = p1

      adjustments (Just a1) Nothing   = (Just a1, Just $ withNewQuantity i2 a1)
      adjustments Nothing   (Just a2) = (Just $ withNewQuantity i1 a2, Just a2)
      adjustments a1        a2        = (a1, a2)

      withNewQuantity i a = a { _itemAdjNewQuantity = if _itemAdjIsDiscontinued a then 0 else _itemQuantity i }

itemProductCode :: OrderItem -> ProductCode
itemProductCode = _productCode . _productInfo . _itemProduct

itemProductName :: OrderItem -> String
itemProductName = _productName . _productInfo . _itemProduct

itemProductPrice :: OrderItem -> Price
itemProductPrice = _productPrice . _productInfo . _itemProduct

itemTotal :: OrderItem -> Money
itemTotal item = atQuantity (_itemQuantity item) (itemProductPrice item)

itemAdjTotal :: OrderItem -> Money
itemAdjTotal i = fromMaybe (itemTotal i) $ fmap itemAdjNewTotal $ _itemAdjustment i

updateItemPrice :: Price -> OrderItem -> OrderItem
updateItemPrice price = itemProduct . productInfo . productPrice .~ price

itemAdjNewTotal :: OrderItemAdjustment -> Money
itemAdjNewTotal adj = atQuantity (_itemAdjNewQuantity adj) (_itemAdjNewPrice adj)

updateItemQuantity :: Maybe Int -> OrderItem -> OrderItem
updateItemQuantity quantity i = i & itemQuantity .~ fromMaybe (_itemQuantity i) quantity

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

removeItemAdjustment :: OrderItem -> OrderItem
removeItemAdjustment = itemAdjustment .~ Nothing

applyItemUpdate :: ProductCatalogue -> OrderItem -> OrderItem
applyItemUpdate catalogue item = case findEntry (itemProductCode item) catalogue of
    Just e | itemProductPrice item == _catalogueEntryPrice e -> item
           | otherwise -> adjustItemPrice date (_catalogueEntryPrice e) item
    _ -> discontinueProduct date item
  where
    date = getDate catalogue

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