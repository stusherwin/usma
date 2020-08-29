{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Utils where

import           Control.Arrow ((&&&))
import           Data.Function (on)
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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f _ [] = []
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
                            
justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing

addOrUpdate :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
addOrUpdate _ _ [] ys = ys
addOrUpdate eq update (x : xs) ys = 
  case partition (eq x) ys of
    (y : _, ys') -> (update x y) : addOrUpdate eq update xs ys' 
    _ -> x : addOrUpdate eq update xs ys

ensure :: (a -> Bool) -> a -> [a] -> [a]
ensure = ensure' False
  where
    ensure' False _ new [] = [new]
    ensure' _ _ _ [] = []
    ensure' found eq new (x : xs)
      | eq x      = x : ensure' True  eq new xs
      | otherwise = x : ensure' found eq new xs

-- update :: (a -> Bool) -> (a -> a) -> [a] -> [a]
-- update _ _ [] = []
-- update cond fn (x:xs) | cond x = fn x : update cond fn xs
--                       | otherwise = x : update cond fn xs

updateWhere :: (a -> Bool) -> (([a] -> Identity [a]) -> b -> Identity b) -> (a -> a) -> b -> b
updateWhere pred lens = over (lens . each . filtered pred)

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .&&. f2 = \x -> f1 x && f2 x
infixr 3 .&&.

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .||. f2 = \x -> f1 x || f2 x
infixr 2 .||.