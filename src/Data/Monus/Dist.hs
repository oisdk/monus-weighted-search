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

import Numeric.Natural ( Natural )
import Data.Bits ( Bits )
import Data.Ix ( Ix )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Data.Monoid ( Sum(Sum) )
import Data.Monus ( Monus )
import Test.QuickCheck
    ( arbitrarySizedNatural, shrinkIntegral, Arbitrary(..) )
import Control.DeepSeq ( NFData )

-- | A very simple 'Monus', based on the addition 'Monoid' on 'Natural' numbers.
-- This represents discrete distances.
newtype Dist = Dist { runDist :: Natural }
  deriving stock (Eq, Ord, Data, Generic, Typeable)
  deriving (Num, Enum, Integral, Show, Read, Real, Ix, Bits, NFData) via Natural
  deriving (Semigroup, Monoid, Monus) via (Sum Natural)

instance Arbitrary Dist where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral

-- | A simple graph with 'Dist'-weighted edges.
--
-- Note that the algorithms in this package can use any monus, not just 'Dist':
-- we specialise here just for simplicity of presentation.
type Graph a = a -> [(a, Dist)]
