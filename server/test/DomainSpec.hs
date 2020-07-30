{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DomainSpec where

import           Control.Arrow ((&&&))
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Test.Hspec
import           Test.Hspec.Wai

import DomainV2

domainSpec :: Spec
domainSpec = 
  describe "reconcile order" $ do
    date <- runIO getCurrentTime
    
    it "should adjust household order items" $ do
      let order = makeOrder date [ (HouseholdId 1, [OrderItemSpec (ProductCode "A123") 100 1])
                                 , (HouseholdId 2, [OrderItemSpec (ProductCode "A123") 100 2])
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
      let order = makeOrder date [ (HouseholdId 1, [OrderItemSpec (ProductCode "A123") 100 1])
                                 , (HouseholdId 2, [OrderItemSpec (ProductCode "A123") 100 2])
                                 ]
      orderItemValues order `shouldBe` [("A123", 100, 3, 300, Nothing, Nothing, Nothing)]
      
      let order' = reconcileOrderItems date [(HouseholdId 1, OrderItemSpec (ProductCode "A123") 100 2)] order
      orderItemValues order' `shouldBe` [("A123", 100, 3, 300, Just 100, Just 4, Just 400)]

      date' <- liftIO getCurrentTime
      let order'' = reconcileOrderItems date' [(HouseholdId 2, OrderItemSpec (ProductCode "A123") 200 2)] order'
      orderItemValues order'' `shouldBe` [("A123", 100, 3, 300, Just 200, Just 4, Just 800)]

    it "should adjust order total" $ do
      let order = makeOrder date [ (HouseholdId 1, [OrderItemSpec (ProductCode "A123") 100 1])
                                 , (HouseholdId 2, [OrderItemSpec (ProductCode "A123") 100 2])
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

makeOrder :: UTCTime -> [(HouseholdId, [OrderItemSpec])] -> Order
makeOrder date householdItems = Order 
    { _orderInfo = orderInfo
    , _orderStatusFlags = orderStatus
    , _orderHouseholdOrders = makeHouseholdOrder orderInfo orderStatus <$> householdItems
    }
  where
    orderInfo = OrderInfo
      { _orderId = OrderId 1
      , _orderGroupId = OrderGroupId 1
      , _orderCreated = date
      , _orderCreatedBy = Nothing
      }
    orderStatus = OrderStatusFlags
      { _orderIsAbandoned = False
      , _orderIsPlaced = True
      }

makeHouseholdOrder :: OrderInfo -> OrderStatusFlags -> (HouseholdId, [OrderItemSpec]) -> HouseholdOrder
makeHouseholdOrder orderInfo orderStatus (householdId, items) = HouseholdOrder 
  { _householdOrderOrderInfo = orderInfo
  , _householdOrderOrderStatusFlags = orderStatus
  , _householdOrderHouseholdInfo = HouseholdInfo 
    { _householdId = householdId
    , _householdName = "Household " ++ (show . fromHouseholdId $ householdId)
    }
  , _householdOrderStatusFlags = HouseholdOrderStatusFlags
    { _householdOrderIsAbandoned = False
    , _householdOrderIsComplete = True
    }
  , _householdOrderItems = makeOrderItem <$> items
  }

makeOrderItem :: OrderItemSpec -> OrderItem
makeOrderItem (OrderItemSpec code price quantity) = OrderItem
  { _itemProduct = Product 
    { _productInfo = ProductInfo
      { _productCode = code
      , _productName = fromProductCode code
      , _productPrice = atVatRate zeroRate price
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
  , _itemQuantity = quantity
  , _itemAdjustment = Nothing
  }