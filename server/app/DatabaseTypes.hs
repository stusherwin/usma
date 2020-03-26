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
  hoidB :: Bool,
  hoidF :: Bool,
  hoidG :: Bool,
  hoidO :: Bool,
  hoidS :: Bool,
  hoidV :: Bool,
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
  oidB :: Bool,
  oidF :: Bool,
  oidG :: Bool,
  oidO :: Bool,
  oidS :: Bool,
  oidV :: Bool
}

instance FromRow OrderItemData where
  fromRow = OrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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
  phoidB :: Bool,
  phoidF :: Bool,
  phoidG :: Bool,
  phoidO :: Bool,
  phoidS :: Bool,
  phoidV :: Bool,
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
  poidB :: Bool,
  poidF :: Bool,
  poidG :: Bool,
  poidO :: Bool,
  poidS :: Bool,
  poidV :: Bool,
  poidOldProductPriceExcVat :: Maybe Int,
  poidOldProductPriceIncVat :: Maybe Int,
  poidOldQuantity :: Maybe Int,
  poidOldItemTotalExcVat :: Maybe Int,
  poidOldItemTotalIncVat :: Maybe Int
}

instance FromRow PastOrderItemData where
  fromRow = PastOrderItemData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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
