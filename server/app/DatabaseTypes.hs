{-# LANGUAGE NamedFieldPuns #-}

module DatabaseTypes where
  
import Control.Monad (mzero, when, void, forM_)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow

import Types

toDatabaseChar :: Char -> Action
toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

instance ToField VatRate where
  toField Zero = toDatabaseChar 'Z'
  toField Standard = toDatabaseChar 'S'
  toField Reduced = toDatabaseChar 'R'

instance FromField VatRate where
  fromField f char = do
    c <- fromField f char
    case c of
      Just 'Z' -> return Zero
      Just 'S' -> return Standard
      Just 'R' -> return Reduced
      _ -> mzero

instance FromRow ProductCatalogueData where
  fromRow = ProductCatalogueData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow ProductCatalogueData where
  toRow e = [ toField $ pcdCode e
            , toField $ pcdCategory e
            , toField $ pcdBrand e
            , toField $ pcdDescription e
            , toField $ pcdText e
            , toField $ pcdSize e
            , toField $ pcdPrice e
            , toField $ pcdVatRate e
            , toField $ pcdRrp e
            , toField $ pcdBiodynamic e
            , toField $ pcdFairTrade e
            , toField $ pcdGlutenFree e
            , toField $ pcdOrganic e
            , toField $ pcdAddedSugar e
            , toField $ pcdVegan e
            , toField $ pcdUpdated e
            ]

data CollectiveOrderData = CollectiveOrderData {
  codId :: Int, 
  codCreated :: UTCTime, 
  codCreatedBy :: Maybe Int, 
  codCreatedByName :: Maybe String, 
  codComplete :: Bool, 
  codOldTotalExcVat :: Int, 
  codOldTotalIncVat :: Int,
  codTotalExcVat :: Int, 
  codTotalIncVat :: Int,
  codAllUpToDate :: Bool
}

instance FromRow CollectiveOrderData where
  fromRow = CollectiveOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data PastCollectiveOrderData = PastCollectiveOrderData {
  pcodOrderId :: Int, 
  pcodOrderCreated :: UTCTime, 
  pcodOrderCreatedBy :: Maybe Int, 
  pcodOrderCreatedByName :: Maybe String, 
  pcodCancelled :: Bool, 
  pcodReconciled :: Bool, 
  pcodTotalExcVat :: Int, 
  pcodTotalIncVat :: Int,
  pcodOldTotalExcVat :: Maybe Int, 
  pcodOldTotalIncVat :: Maybe Int
}

instance FromRow PastCollectiveOrderData where
  fromRow = PastCollectiveOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 

data HouseholdOrderData = HouseholdOrderData {
  hodOrderId :: Int, 
  hodOrderCreated :: UTCTime, 
  hodOrderCreatedBy :: Maybe Int, 
  hodOrderCreatedByName :: Maybe String, 
  hodHouseholdId :: Int, 
  hodHouseholdName :: String, 
  hodComplete :: Bool, 
  hodCancelled :: Bool, 
  hodOldTotalExcVat :: Int, 
  hodOldTotalIncVat :: Int,
  hodTotalExcVat :: Int, 
  hodTotalIncVat :: Int,
  hodUpdated :: Bool
}

instance FromRow HouseholdOrderData where
  fromRow = HouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data PastHouseholdOrderData = PastHouseholdOrderData {
  phodOrderId :: Int, 
  phodOrderCreated :: UTCTime, 
  phodOrderCreatedBy :: Maybe Int, 
  phodOrderCreatedByName :: Maybe String, 
  phodOrderAbandoned :: Bool,
  phodHouseholdId :: Int, 
  phodHouseholdName :: String, 
  phodCancelled :: Bool, 
  phodReconciled :: Bool, 
  phodTotalExcVat :: Int, 
  phodTotalIncVat :: Int,
  phodOldTotalExcVat :: Maybe Int, 
  phodOldTotalIncVat :: Maybe Int
}

instance FromRow PastHouseholdOrderData where
  fromRow = PastHouseholdOrderData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 

data CollectiveOrderItemData = CollectiveOrderItemData {
  oidOrderId :: Int,
  oidProductId :: Int,
  oidCode :: String,
  oidName :: String,
  oidVatRate :: VatRate,
  oidQuantity :: Int,
  oidPriceExcVat :: Int,
  oidPriceIncVat :: Int,
  oidItemTotalExcVat :: Int,
  oidItemTotalIncVat :: Int,
  oidBiodynamic :: Bool,
  oidFairTrade :: Bool,
  oidGlutenFree :: Bool,
  oidOrganic :: Bool,
  oidAddedSugar :: Bool,
  oidVegan :: Bool
}

instance FromRow CollectiveOrderItemData where
  fromRow = CollectiveOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data PastOrderItemData = PastOrderItemData {
  poidProductId :: Int,
  poidCode :: String,
  poidName :: String,
  poidPriceExcVat :: Int,
  poidPriceIncVat :: Int,
  poidVatRate :: VatRate,
  poidQuantity :: Int,
  poidItemTotalExcVat :: Int,
  poidItemTotalIncVat :: Int,
  poidBiodynamic :: Bool,
  poidFairTrade :: Bool,
  poidGlutenFree :: Bool,
  poidOrganic :: Bool,
  poidAddedSugar :: Bool,
  poidVegan :: Bool,
  poidOldPriceExcVat :: Maybe Int,
  poidOldPriceIncVat :: Maybe Int,
  poidOldQuantity :: Maybe Int,
  poidOldItemTotalExcVat :: Maybe Int,
  poidOldItemTotalIncVat :: Maybe Int
}

instance FromRow PastOrderItemData where
  fromRow = PastOrderItemData <$> {-field <*>-} field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data HouseholdOrderItemData = HouseholdOrderItemData {
  hoidOrderId :: Int,
  hoidHouseholdId :: Int,

  hoidProductId :: Int,
  hoidCode :: String,
  hoidName :: String,
  hoidPriceExcVat :: Int,
  hoidPriceIncVat :: Int,
  hoidVatRate :: VatRate,
  hoidQuantity :: Int,
  hoidItemTotalExcVat :: Int,
  hoidItemTotalIncVat :: Int,
  hoidBiodynamic :: Bool,
  hoidFairTrade :: Bool,
  hoidGlutenFree :: Bool,
  hoidOrganic :: Bool,
  hoidAddedSugar :: Bool,
  hoidVegan :: Bool,
  hoidOldPriceExcVat :: Int,
  hoidOldPriceIncVat :: Int,
  hoidOldItemTotalExcVat :: Int,
  hoidOldItemTotalIncVat :: Int,
  hoidProductDiscontinued :: Bool,
  
  hoidUpdated :: Bool
}

instance FromRow HouseholdOrderItemData where
  fromRow = HouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
             
data ProductCatalogueEntryData = ProductCatalogueEntryData {
  pcedCode :: String,
  pcedName :: String,
  pcedPriceExcVat :: Int,
  pcedPriceIncVat :: Int,
  pcedVatRate :: VatRate,
  pcedBiodynamic :: Bool,
  pcedFairTrade :: Bool,
  pcedGlutenFree :: Bool,
  pcedOrganic :: Bool,
  pcedAddedSugar :: Bool,
  pcedVegan :: Bool,
  pcedCategory :: String,
  pcedBrand :: String
}

instance FromRow ProductCatalogueEntryData where
  fromRow = ProductCatalogueEntryData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

fromCollectiveOrderData :: [CollectiveOrderItemData] -> CollectiveOrderData -> CollectiveOrder
fromCollectiveOrderData items (d@CollectiveOrderData { codAllUpToDate = True }) 
  = CollectiveOrder (codId d) 
                    (codCreated d) 
                    (codCreatedBy d) 
                    (codCreatedByName d) 
                    False 
                    False 
                    (codComplete d) 
                    (codTotalExcVat d) 
                    (codTotalIncVat d) 
                    True 
                    Nothing 
                    $ map fromCollectiveOrderItemData $ filter (\i -> oidOrderId i == codId d) items

fromCollectiveOrderData items d
  = CollectiveOrder (codId d)
                    (codCreated d)
                    (codCreatedBy d)
                    (codCreatedByName d)
                    False 
                    False 
                    (codComplete d)
                    (codTotalExcVat d)
                    (codTotalIncVat d)
                    False 
                    (Just $ OrderAdjustment (codOldTotalExcVat d) (codOldTotalIncVat d))
                    $ map fromCollectiveOrderItemData $ filter (\i -> oidOrderId i == codId d) items

fromPastCollectiveOrderData :: [(Int, PastOrderItemData)] -> PastCollectiveOrderData -> PastCollectiveOrder
fromPastCollectiveOrderData items (d@PastCollectiveOrderData { pcodOldTotalExcVat = Just oldTotalExcVat
                                                             , pcodOldTotalIncVat = Just oldTotalIncVat })
  = PastCollectiveOrder (pcodOrderId d)
                        (pcodOrderCreated d)
                        (pcodOrderCreatedBy d)
                        (pcodOrderCreatedByName d)
                        (not $ pcodCancelled d)
                        (pcodCancelled d)
                        (pcodCancelled d)
                        (not $ pcodCancelled d)
                        (pcodReconciled d)
                        (pcodTotalExcVat d)
                        (pcodTotalIncVat d)
                        True 
                        (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat) 
                        $ map (fromPastOrderItemData . snd) $ filter (\(orderId, _) -> orderId == pcodOrderId d) items

fromPastCollectiveOrderData items d 
  = PastCollectiveOrder (pcodOrderId d)
                        (pcodOrderCreated d)
                        (pcodOrderCreatedBy d)
                        (pcodOrderCreatedByName d)
                        (not $ pcodCancelled d)
                        (pcodCancelled d)
                        (pcodCancelled d)
                        (not $ pcodCancelled d)
                        (pcodReconciled d)
                        (pcodTotalExcVat d)
                        (pcodTotalIncVat d)
                        True 
                        Nothing
                        $ map (fromPastOrderItemData . snd) $ filter (\(orderId, _) -> orderId == pcodOrderId d) items

fromHouseholdOrderData :: [HouseholdOrderItemData] -> HouseholdOrderData -> HouseholdOrder
fromHouseholdOrderData items d 
  = HouseholdOrder (hodOrderId d) 
                   (hodOrderCreated d)
                   (hodOrderCreatedBy d)
                   (hodOrderCreatedByName d)
                   False 
                   False 
                   (hodHouseholdId d)
                   (hodHouseholdName d)
                   (hodComplete d)
                   (hodCancelled d)
                   (not (hodComplete d) && not (hodCancelled d))
                   (hodTotalExcVat d)
                   (hodTotalIncVat d)
                   (if hodUpdated d then Just $ OrderAdjustment (hodOldTotalExcVat d) (hodOldTotalIncVat d) else Nothing)
                   $ map fromHouseholdOrderItemData $ filter (\i -> hoidOrderId i == hodOrderId d && hoidHouseholdId i == hodHouseholdId d) items

fromPastHouseholdOrderData :: [((Int, Int), PastOrderItemData)] -> PastHouseholdOrderData -> PastHouseholdOrder
fromPastHouseholdOrderData items (d@PastHouseholdOrderData { phodOldTotalExcVat = Just oldTotalExcVat
                                                           , phodOldTotalIncVat = Just oldTotalIncVat })
  = PastHouseholdOrder (phodOrderId d) 
                       (phodOrderCreated d)
                       (phodOrderCreatedBy d) 
                       (phodOrderCreatedByName d) 
                       (not $ phodOrderAbandoned d)
                       (phodOrderAbandoned d)
                       (phodHouseholdId d)
                       (phodHouseholdName d)
                       (phodCancelled d)
                       (not $ phodCancelled d) 
                       False 
                       (phodReconciled d)
                       (phodTotalExcVat d)
                       (phodTotalIncVat d)
                       (Just $ OrderAdjustment oldTotalExcVat oldTotalIncVat)
                       $ map (fromPastOrderItemData . snd) $ filter (\((orderId, householdId), _) -> orderId == phodOrderId d && householdId == phodHouseholdId d) items

fromPastHouseholdOrderData items d
  = PastHouseholdOrder (phodOrderId d) 
                       (phodOrderCreated d)
                       (phodOrderCreatedBy d) 
                       (phodOrderCreatedByName d) 
                       (not $ phodOrderAbandoned d)
                       (phodOrderAbandoned d)
                       (phodHouseholdId d)
                       (phodHouseholdName d)
                       (phodCancelled d)
                       (not $ phodCancelled d) 
                       False 
                       (phodReconciled d)
                       (phodTotalExcVat d)
                       (phodTotalIncVat d)
                       Nothing
                       $ map (fromPastOrderItemData . snd) $ filter (\((orderId, householdId), _) -> orderId == phodOrderId d && householdId == phodHouseholdId d) items

fromCollectiveOrderItemData :: CollectiveOrderItemData -> OrderItem
fromCollectiveOrderItemData d 
  = OrderItem (oidProductId d)
              (oidCode d)
              (oidName d)
              (oidVatRate d)
              (oidPriceExcVat d)
              (oidPriceIncVat d)
              (oidQuantity d)
              (oidItemTotalExcVat d)
              (oidItemTotalIncVat d)
              (oidBiodynamic d)
              (oidFairTrade d)
              (oidGlutenFree d)
              (oidOrganic d)
              (oidAddedSugar d)
              (oidVegan d)
              Nothing

fromPastOrderItemData :: PastOrderItemData -> OrderItem
fromPastOrderItemData (d@PastOrderItemData { poidOldPriceExcVat = Just oldProductPriceExcVat
                                           , poidOldPriceIncVat = Just oldProductPriceIncVat
                                           , poidOldQuantity = Just oldQuantity
                                           , poidOldItemTotalExcVat = Just oldItemTotalExcVat
                                           , poidOldItemTotalIncVat = Just oldItemTotalIncVat
                                           })
  = OrderItem (poidProductId d)
              (poidCode d)
              (poidName d)
              (poidVatRate d)
              (poidPriceExcVat d)
              (poidPriceIncVat d)
              (poidQuantity d)
              (poidItemTotalExcVat d)
              (poidItemTotalIncVat d)
              (poidBiodynamic d)
              (poidFairTrade d)
              (poidGlutenFree d)
              (poidOrganic d)
              (poidAddedSugar d)
              (poidVegan d)
              $ Just $ OrderItemAdjustment oldProductPriceExcVat 
                                           oldProductPriceIncVat 
                                           oldQuantity 
                                           oldItemTotalExcVat 
                                           oldItemTotalIncVat 
                                           False

fromPastOrderItemData d 
  = OrderItem (poidProductId d)
              (poidCode d)
              (poidName d)
              (poidVatRate d)
              (poidPriceExcVat d)
              (poidPriceIncVat d)
              (poidQuantity d)
              (poidItemTotalExcVat d)
              (poidItemTotalIncVat d)
              (poidBiodynamic d)
              (poidFairTrade d)
              (poidGlutenFree d)
              (poidOrganic d)
              (poidAddedSugar d)
              (poidVegan d)
              Nothing

fromHouseholdOrderItemData :: HouseholdOrderItemData -> OrderItem
fromHouseholdOrderItemData d 
  = OrderItem (hoidProductId d)
              (hoidCode d)
              (hoidName d)
              (hoidVatRate d)
              (hoidPriceExcVat d)
              (hoidPriceIncVat d)
              (hoidQuantity d)
              (hoidItemTotalExcVat d)
              (hoidItemTotalIncVat d)
              (hoidBiodynamic d)
              (hoidFairTrade d)
              (hoidGlutenFree d)
              (hoidOrganic d)
              (hoidAddedSugar d)
              (hoidVegan d)
              $ if (hoidUpdated d) 
                then Just $ OrderItemAdjustment (hoidOldPriceExcVat d)
                                                (hoidOldPriceIncVat d)
                                                (hoidQuantity d)
                                                (hoidOldItemTotalExcVat d)
                                                (hoidOldItemTotalIncVat d)
                                                (hoidProductDiscontinued d)
                else Nothing

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
