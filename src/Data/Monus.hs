module Data.Monus (Monus(..)) where

import Data.Monoid

infixl 6 |-|
class (Ord a, Monoid a) => Monus a where (|-|) :: a -> a -> a

instance (Num a, Ord a) => Monus (Sum a) where
  x |-| y
    | x <= y = y - x
    | otherwise = x - y
  {-# INLINE (|-|) #-}
