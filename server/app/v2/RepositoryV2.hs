{-# LANGUAGE DuplicateRecordFields #-}

module RepositoryV2 where

import Database.PostgreSQL.Simple

import DomainV2
import DatabaseV2
import Data.Maybe (listToMaybe)
import Config
import Prelude hiding (sum)

getCollectiveOrder :: Config -> Int -> IO (Maybe Order)
getCollectiveOrder config groupId = do
  conn <- connectPostgreSQL $ connectionString config
  (rVatRates, rOrders, rItems) <- withTransaction conn $ do
    v <- getVatRateData conn groupId
    o <- getCollectiveOrderData conn groupId
    i <- getCollectiveOrderItemData conn groupId
    return (v, o, i)
  close conn
  return $ listToMaybe $ map (toOrder rVatRates rItems) rOrders

toOrder :: VatRates -> [(Int, OrderItemData)] -> CollectiveOrderData -> Order
toOrder vatRates rItems o = Order 
  { orderId          = OrderId $ order_id o
  , orderCreated     = order_created o
  , orderCreatedBy   = toHouseholdInfo o
  , orderIsPlaced    = False
  , orderIsAbandoned = False
  , orderIsComplete  = False -- todo
  , orderTotal       = sum $ map itemTotal items
  , orderIsAllHouseholdsUpToDate = True -- todo
  , orderAdjustment  = Nothing -- todo
  , orderItems       = items
  }
  where
  items = map ((toItem vatRates) . snd) $ filter ((order_id o == ) . fst) rItems


toItem :: VatRates -> OrderItemData -> OrderItem
toItem vatRates i = OrderItem 
  { itemProduct    = toProduct vatRates i
  , itemQuantity   = item_quantity i
  , itemTotal      = value' (product_vat_rate i) (product_price i * item_quantity i)
  , itemAdjustment = Nothing -- todo
  }
  where value' = value vatRates

toProduct :: VatRates -> OrderItemData -> Product
toProduct vatRates i = Product 
  { productId           = ProductId $ product_id i
  , productCode         = product_code i
  , productName         = product_name i
  , productVatRate      = product_vat_rate i
  , productPrice        = value' (product_vat_rate i) (product_price i)
  , productIsBiodynamic = product_biodynamic i
  , productIsFairTrade  = product_fair_trade i
  , productIsGlutenFree = product_gluten_free i
  , productIsOrganic    = product_organic i
  , productIsAddedSugar = product_added_sugar i
  , productIsVegan      = product_vegan i
  }
  where value' = value vatRates

toHouseholdInfo :: CollectiveOrderData -> Maybe HouseholdInfo
toHouseholdInfo (CollectiveOrderData { order_created_by_household_id = Just id
                                     , order_created_by_household_name = Just name
                                     })
  = Just $ HouseholdInfo (HouseholdId id) name
toHouseholdInfo _ = Nothing