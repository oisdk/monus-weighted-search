--------------------------------------------------------------------------------
-- |
-- Module      : Data.Monus.Dist
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A 'Monus' for discrete distances.
--------------------------------------------------------------------------------

module Data.Monus.Dist where

import Numeric.Natural
import Data.Bits
import Data.Ix
import Data.Data
import GHC.Generics
import Data.Monoid
import Data.Monus
import Test.QuickCheck
import Control.DeepSeq

-- | A very simple 'Monus', based on the addition 'Monoid' on 'Natural' numbers.
-- This represents discrete distances.
newtype Dist = Dist { runDist :: Natural }
  deriving stock (Eq, Ord, Data, Generic, Typeable)
  deriving (Num, Enum, Integral, Show, Read, Real, Ix, Bits, NFData) via Natural
  deriving (Semigroup, Monoid, Monus) via (Sum Natural)

instance Arbitrary Dist where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral
