{-# LANGUAGE DuplicateRecordFields #-}

module RepositoryV2 where

import Database.PostgreSQL.Simple

import DomainV2
import DatabaseV2
import Data.Maybe (listToMaybe)
import Config

getCollectiveOrder :: Config -> Int -> IO (Maybe Order)
getCollectiveOrder config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (vatRates, orders, items) <- withTransaction conn $ do
    v <- getVatRateData conn groupId
    o <- getCollectiveOrderData conn groupId
    i <- getCollectiveOrderItemData conn groupId
    return (v, o, i)
  close conn
  return $ listToMaybe $ map (toOrder vatRates items) orders

toOrder :: VatRates -> [(Int, OrderItemData)] -> CollectiveOrderData -> Order
toOrder vatRates items o = Order 
  { orderId     = OrderId $ order_id o
  , created     = created (o :: CollectiveOrderData)
  , createdBy   = toHouseholdInfo o
  , isPlaced    = False
  , isAbandoned = False
  , isComplete  = False -- todo
  , total       = zero -- todo
  , isAllHouseholdsUpToDate = True -- todo
  , adjustment  = Nothing -- todo
  , items       = map ((toItem vatRates) . snd) $ filter ((order_id o == ) . fst) items
  }

toItem :: VatRates -> OrderItemData -> OrderItem
toItem vatRates i = OrderItem 
  { product    = toProduct vatRates i
  , quantity   = quantity (i :: OrderItemData)
  , total      = vatMoney (vat_rate i) (price (i :: OrderItemData) * quantity (i :: OrderItemData))
  , adjustment = Nothing -- todo
  }
  where vatMoney = money vatRates

toProduct :: VatRates -> OrderItemData -> Product
toProduct vatRates i = Product 
  { productId = ProductId $ product_id i
  , code      = code (i :: OrderItemData)
  , name      = name (i :: OrderItemData)
  , vatRate   = vat_rate i
  , price     = vatMoney (vat_rate i) (price (i :: OrderItemData))
  , isBiodynamic = biodynamic i
  , isFairTrade  = fair_trade i
  , isGlutenFree = gluten_free i
  , isOrganic    = organic i
  , isAddedSugar = added_sugar i
  , isVegan      = vegan i
  }
  where vatMoney = money vatRates

toHouseholdInfo :: CollectiveOrderData -> Maybe HouseholdInfo
toHouseholdInfo (CollectiveOrderData { created_by_household_id = Just id
                                     , created_by_household_name = Just name
                                     })
  = Just $ HouseholdInfo (HouseholdId id) name
toHouseholdInfo _ = Nothing