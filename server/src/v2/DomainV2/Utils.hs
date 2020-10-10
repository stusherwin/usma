{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainV2.Utils where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f _ [] = []
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
                            
justWhen :: a -> Bool -> Maybe a
justWhen a condition = if condition then Just a else Nothing

ensure :: (a -> Bool) -> a -> [a] -> [a]
ensure = ensure' False
  where
    ensure' False _ new [] = [new]
    ensure' _ _ _ [] = []
    ensure' found eq new (x : xs)
      | eq x      = x : ensure' True  eq new xs
      | otherwise = x : ensure' found eq new xs

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .&&. f2 = \x -> f1 x && f2 x
infixr 3 .&&.

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f1 .||. f2 = \x -> f1 x || f2 x
infixr 2 .||.