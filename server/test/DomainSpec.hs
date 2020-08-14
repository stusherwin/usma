{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DomainSpec where

import           Control.Arrow ((&&&))
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Test.Hspec
import           Test.Hspec.Wai

import DomainV2

domainSpec :: Spec
domainSpec = do
  date <- runIO getCurrentTime

  describe "orderTotal" $ do
    it "should calculate vat for single item to nearest penny" $ do
      let order = makeOrder date OrderOpen [(HouseholdId 1, HouseholdOrderOpen, [("A123", (atVatRate standardRate 101), 1)])]
      
      orderTotal order `shouldBe` (Money 101 121)

    it "should calculate vat of multiple items total to nearest penny" $ do
      let order = makeOrder date OrderOpen [(HouseholdId 1, HouseholdOrderOpen, [("A123", (atVatRate standardRate 101), 10)])]
      
      orderTotal order `shouldBe` (Money 1010 1212)

    it "should calculate vat of multiple items total to nearest penny per order line not across whole order" $ do
      let order = makeOrder date OrderOpen [(HouseholdId 1, HouseholdOrderOpen, [ ("DY009", (atVatRate standardRate 1944), 1) -- 1944  2333
                                                                                , ("DY026", (atVatRate standardRate 2500), 1) -- 2500  3000
                                                                                , ("ZX435", (atVatRate standardRate 1646), 3) -- 4938  5926
                                                                                ])]                                           -- ==== =====
                                                                                                                              -- 9382 11259
      orderTotal order `shouldBe` (Money 9382 11259) -- not 9382 11258 (9382 * 1.2 = 11258.4)

  describe "reconcile order" $ do
    it "should adjust household order items" $ do
      let order = makeOrder date OrderPlaced [ (HouseholdId 1, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 1)])
                                             , (HouseholdId 2, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 2)])
                                             ]
      householdOrderItemValues order `shouldBe` [ (HouseholdId 1, [("A123", 100, 1, 100, Nothing, Nothing, Nothing)])
                                                , (HouseholdId 2, [("A123", 100, 2, 200, Nothing, Nothing, Nothing)])
                                                ]
      
      let order' = reconcileOrderItems date [(HouseholdId 1, OrderItemSpec (ProductCode "A123") 100 2)] order
      householdOrderItemValues order' `shouldBe` [ (HouseholdId 1, [("A123", 100, 1, 100, Just 100, Just 2, Just 200)])
                                                 , (HouseholdId 2, [("A123", 100, 2, 200, Nothing, Nothing, Nothing)])
                                                 ]

      date' <- liftIO getCurrentTime
      let order'' = reconcileOrderItems date' [(HouseholdId 2, OrderItemSpec (ProductCode "A123") 200 2)] order'
      householdOrderItemValues order'' `shouldBe` [ (HouseholdId 1, [("A123", 100, 1, 100, Just 100, Just 2, Just 200)])
                                                  , (HouseholdId 2, [("A123", 100, 2, 200, Just 200, Just 2, Just 400)])
                                                  ]

    it "should adjust order items" $ do
      let order = makeOrder date OrderPlaced [ (HouseholdId 1, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 1)])
                                             , (HouseholdId 2, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 2)])
                                             ]
      orderItemValues order `shouldBe` [("A123", 100, 3, 300, Nothing, Nothing, Nothing)]
      
      let order' = reconcileOrderItems date [(HouseholdId 1, OrderItemSpec (ProductCode "A123") 100 2)] order
      orderItemValues order' `shouldBe` [("A123", 100, 3, 300, Just 100, Just 4, Just 400)]

      date' <- liftIO getCurrentTime
      let order'' = reconcileOrderItems date' [(HouseholdId 2, OrderItemSpec (ProductCode "A123") 200 2)] order'
      orderItemValues order'' `shouldBe` [("A123", 100, 3, 300, Just 200, Just 4, Just 800)]

    it "should adjust order total" $ do
      let order = makeOrder date OrderPlaced [ (HouseholdId 1, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 1)])
                                             , (HouseholdId 2, HouseholdOrderComplete, [("A123", (atVatRate zeroRate 100), 2)])
                                             ]
      orderTotal order `shouldBe` 300
      orderAdjustment order `shouldBe` Nothing
      
      let order' = reconcileOrderItems date [(HouseholdId 1, OrderItemSpec (ProductCode "A123") 100 2)] order
      
      orderTotal order' `shouldBe` 300
      orderAdjustment order' `shouldBe` Just (OrderAdjustment 400)

      date' <- liftIO getCurrentTime
      let order'' = reconcileOrderItems date' [(HouseholdId 2, OrderItemSpec (ProductCode "A123") 200 2)] order'
      
      orderTotal order'' `shouldBe` 300
      orderAdjustment order'' `shouldBe` Just (OrderAdjustment 600)

type OrderItemValues = (String, Money, Int, Money, Maybe Money, Maybe Int, Maybe Money)

householdOrderItemValues :: Order -> [(HouseholdId, [OrderItemValues])]
householdOrderItemValues = map (householdOrderHouseholdId &&& map itemValues . _householdOrderItems) . _orderHouseholdOrders

orderItemValues :: Order -> [OrderItemValues]
orderItemValues = map itemValues . orderItems

itemValues :: OrderItem -> OrderItemValues
itemValues i = ( fromProductCode . itemProductCode $ i
               , _priceAmount . itemProductPrice $ i
               , _itemQuantity i
               , itemTotal i
               , _priceAmount . _itemAdjNewPrice <$> (_itemAdjustment i)
               , _itemAdjNewQuantity <$> (_itemAdjustment i)
               , itemAdjNewTotal <$> (_itemAdjustment i)
               )

makeOrder :: UTCTime -> OrderStatus -> [(HouseholdId, HouseholdOrderStatus, [(String, Price, Int)])] -> Order
makeOrder date status householdItems = Order 
    { _orderInfo = orderInfo
    , _orderStatus = status
    , _orderHouseholdOrders = makeHouseholdOrder orderInfo status <$> householdItems
    }
  where
    orderInfo = OrderInfo
      { _orderId = OrderId 1
      , _orderGroupId = OrderGroupId 1
      , _orderCreated = date
      , _orderCreatedBy = Nothing
      }

makeHouseholdOrder :: OrderInfo -> OrderStatus -> (HouseholdId, HouseholdOrderStatus, [(String, Price, Int)]) -> HouseholdOrder
makeHouseholdOrder orderInfo orderStatus (householdId, status, items) = HouseholdOrder 
  { _householdOrderOrderInfo = orderInfo
  , _householdOrderOrderStatus = orderStatus
  , _householdOrderHouseholdInfo = HouseholdInfo 
    { _householdId = householdId
    , _householdName = "Household " ++ (show . fromHouseholdId $ householdId)
    }
  , _householdOrderStatus = status
  , _householdOrderItems = makeOrderItem <$> items
  }

makeOrderItem :: (String, Price, Int) -> OrderItem
makeOrderItem (code, price, quantity) = OrderItem
  { _itemProduct = makeProduct (code, price)
  , _itemQuantity = quantity
  , _itemAdjustment = Nothing
  }

makeProduct :: (String, Price) -> Product
makeProduct (code, price) = Product 
  { _productInfo = ProductInfo
    { _productCode = ProductCode code
    , _productName = code
    , _productPrice = price
    }
  , _productFlags = ProductFlags
    { _productIsBiodynamic = False
    , _productIsFairTrade = False
    , _productIsGlutenFree = False
    , _productIsOrganic = False
    , _productIsAddedSugar = False
    , _productIsVegan = False
    }
  }