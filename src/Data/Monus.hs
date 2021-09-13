--------------------------------------------------------------------------------
-- |
-- Module      : Data.Monus
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A class for 'Monoid's with an order and a pseudo-subtraction operator.
--------------------------------------------------------------------------------

module Data.Monus (Monus(..)) where

import Data.Monoid (Sum(..), Any(..))

infixl 6 |-|
-- | A class for (constructive) totally-ordered commutative monoids. These
-- are monoids such that their ordering respects the '<>' operator, meaning
-- they obey the following law:
--
-- @
--   x '<=' x '<>' y
-- @
--
-- These monoids must also have a pseudo-subtraction operator ('|-|'), which
-- behaves like an absolute difference function. This operator must obey the
-- following law:
--
-- @
--   x '<=' y ==> x '<>' (y '|-|' x) '==' y
-- @
  
class (Ord a, Monoid a) => Monus a where
  -- | An absolute difference operator.
  (|-|) :: a -> a -> a

instance (Num a, Ord a) => Monus (Sum a) where
  Sum x |-| Sum y
    | x <= y = Sum (y - x)
    | otherwise = Sum (x - y)
  {-# INLINE (|-|) #-}

-- |
--
-- >>> let bools = [Any False, Any True]
-- >>> and [ x <> (y |-| x) == y | x <- bools, y <- bools, x <= y]
-- True
instance Monus Any where
  (|-|) = (<>)
  {-# INLINE (|-|) #-}

-- F F : F <= F ==> F || F || F == F
-- F T : F <= T ==> F || T || F == T
-- T T : T <= T ==> T || T || T == T
-- T F : T <= F ==> T || F || T == F
