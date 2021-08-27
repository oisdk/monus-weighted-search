-- | 

module Data.Monus.Dist where

import Numeric.Natural
import Data.Bits
import Data.Ix
import Data.Data
import GHC.Generics

newtype Dist = Dist { runDist :: Natural }
  deriving stock (Eq, Ord, Data, Generic, Typeable)
  deriving (Num, Enum, Integral, Show, Read, Real, Ix, Bits) via Natural
  
