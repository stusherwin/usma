{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Catalogue where

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
import DomainV2.Prices

instance Hashable ProductCode

productCatalogue :: [ProductCatalogueEntry] -> ProductCatalogue
productCatalogue = ProductCatalogue . H.fromList . map (_catalogueEntryCode &&& id)

findEntry :: ProductCode -> ProductCatalogue -> Maybe ProductCatalogueEntry
findEntry code = H.lookup code . fromProductCatalogue

getEntries :: ProductCatalogue -> [ProductCatalogueEntry]
getEntries = sortBy (compare `on` _catalogueEntryCode) . H.elems . fromProductCatalogue

--TODO: Safe version? If catalogue empty
getDate :: ProductCatalogue -> UTCTime
getDate = _catalogueEntryUpdated . head . H.elems . fromProductCatalogue

parseCatalogue :: [VatRate] -> UTCTime -> String -> ProductCatalogue
parseCatalogue vatRates date file =
      productCatalogue
    . catMaybes 
    . map parse 
    . map (splitOn ',')
    . drop 1 
    . lines 
    $ file
  where
    parse [cat,brand,code,desc,text,size,price,vat,_,b,f,g,o,s,v,_] = 
      let code' = ProductCode $ trim code
          vatRateType = case trim vat of
            "1" -> Standard
            "5" -> Reduced
            _ -> Zero
          vatRate = getVatRate vatRateType vatRates     
          price' = atVatRate vatRate $ fromMaybe 0 $ round . (* 100) <$> (readMaybe (trim price) :: Maybe Float)
          rrp' =  round . (* 100) <$> (readMaybe (trim price) :: Maybe Float)
          b' = trim b == "B"
          f' = trim f == "F"
          g' = trim g == "G"
          o' = trim o == "O"
          s' = trim s == "S"
          v' = trim v == "V"
      in  Just $ ProductCatalogueEntry code' cat brand desc text size price' rrp' b' f' g' o' s' v' date
    parse _ = Nothing

fromCatalogueEntry :: ProductCatalogueEntry -> Product
fromCatalogueEntry e = Product 
  { _productInfo = ProductInfo
    { _productCode = _catalogueEntryCode e
    , _productName = buildProductName e
    , _productPrice = _catalogueEntryPrice e
    }
  , _productFlags = ProductFlags
    { _productIsBiodynamic = _catalogueEntryBiodynamic e
    , _productIsFairTrade = _catalogueEntryFairTrade e
    , _productIsGlutenFree = _catalogueEntryGlutenFree e
    , _productIsOrganic = _catalogueEntryOrganic e
    , _productIsAddedSugar = _catalogueEntryAddedSugar e
    , _productIsVegan = _catalogueEntryVegan e
    }
  }

buildProductName :: ProductCatalogueEntry -> String
buildProductName e = unwords
              $ filter (not . null)
              [ _catalogueEntryBrand e
              , _catalogueEntryDescription $ e
              , if null (_catalogueEntrySize e)
                  then ""
                  else "(" ++ (lower . _catalogueEntrySize $ e) ++ ")"
              , _catalogueEntryText $ e
              ]