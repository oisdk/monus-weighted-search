-- | 

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

newtype Dist = Dist { runDist :: Natural }
  deriving stock (Eq, Ord, Data, Generic, Typeable)
  deriving (Num, Enum, Integral, Show, Read, Real, Ix, Bits, NFData) via Natural
  deriving (Semigroup, Monoid, Monus) via (Sum Natural)

instance Arbitrary Dist where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral
