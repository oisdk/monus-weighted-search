module Data.Monus.Prob where

import Data.Ratio
import Data.Monus
import Numeric.Natural
import Data.Ord
import Data.Monoid
import Test.QuickCheck

import Data.Data
import GHC.Generics

import Control.Applicative

newtype Prob = Prob { runProb :: Ratio Natural }
  deriving stock (Eq, Data, Generic, Typeable)
  deriving (Num, Fractional, Show, Read, Real, RealFrac) via Ratio Natural
  deriving Ord via Down (Ratio Natural)
  deriving (Semigroup, Monoid) via Product (Ratio Natural)

instance Monus Prob where
  x |-| y = case compare x y of
    LT -> y / x
    EQ -> 1
    GT -> x / y
  {-# INLINE (|-|) #-}

instance Arbitrary Prob where
  arbitrary = liftA2 f arbitrary arbitrary
    where
      f (NonNegative n) (Positive d) = Prob (fromInteger n % fromInteger (n + d))

instance Bounded Prob where
  minBound = 0
  maxBound = 1
