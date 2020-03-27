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

data HouseholdOrderItemData = HouseholdOrderItemData {
  hoidOrderId :: Int,
  hoidHouseholdId :: Int,
  hoidProductId :: Int,
  hoidCode :: String,
  hoidName :: String,
  hoidOldPriceExcVat :: Int,
  hoidOldPriceIncVat :: Int,
  hoidVatRate :: VatRate,
  hoidQuantity :: Int,
  hoidOldItemTotalExcVat :: Int,
  hoidOldItemTotalIncVat :: Int,
  hoidDiscontinued :: Bool,
  hoidPriceExcVat :: Int,
  hoidPriceIncVat :: Int,
  hoidItemTotalExcVat :: Int,
  hoidItemTotalIncVat :: Int,
  hoidBiodynamic :: Bool,
  hoidFairTrade :: Bool,
  hoidGlutenFree :: Bool,
  hoidOrganic :: Bool,
  hoidAddedSugar :: Bool,
  hoidVegan :: Bool,
  hoidUpdated :: Bool
}

instance FromRow HouseholdOrderItemData where
  fromRow = HouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data OrderItemData = OrderItemData {
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

instance FromRow OrderItemData where
  fromRow = OrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

fromOrderItemData :: OrderItemData -> OrderItem
fromOrderItemData d = OrderItem (oidProductId d)
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
             
data PastHouseholdOrderItemData = PastHouseholdOrderItemData {
  phoidOrderId :: Int,
  phoidHouseholdId :: Int,
  phoidProductId :: Int,
  phoidCode :: String,
  phoidName :: String,
  phoidPriceExcVat :: Int,
  phoidPriceIncVat :: Int,
  phoidVatRate :: VatRate,
  phoidQuantity :: Int,
  phoidItemTotalExcVat :: Int,
  phoidItemTotalIncVat :: Int,
  phoidBiodynamic :: Bool,
  phoidFairTrade :: Bool,
  phoidGlutenFree :: Bool,
  phoidOrganic :: Bool,
  phoidAddedSugar :: Bool,
  phoidVegan :: Bool,
  phoidOldProductPriceExcVat :: Maybe Int,
  phoidOldProductPriceIncVat :: Maybe Int,
  phoidOldQuantity :: Maybe Int,
  phoidOldItemTotalExcVat :: Maybe Int,
  phoidOldItemTotalIncVat :: Maybe Int
}

instance FromRow PastHouseholdOrderItemData where
  fromRow = PastHouseholdOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data PastOrderItemData = PastOrderItemData {
  poidOrderId :: Int,
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
  poidOldProductPriceExcVat :: Maybe Int,
  poidOldProductPriceIncVat :: Maybe Int,
  poidOldQuantity :: Maybe Int,
  poidOldItemTotalExcVat :: Maybe Int,
  poidOldItemTotalIncVat :: Maybe Int
}

instance FromRow PastOrderItemData where
  fromRow = PastOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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

fromCollectiveOrderData :: CollectiveOrderData -> [OrderItem] -> CollectiveOrder
fromCollectiveOrderData (d@CollectiveOrderData { codAllUpToDate = True }) items =
  CollectiveOrder (codId d) 
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
                  items

fromCollectiveOrderData d items =
  CollectiveOrder (codId d)
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
                  items

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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <&>

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixr 0 &
