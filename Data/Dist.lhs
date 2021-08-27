%include polycode.fmt
%include forall.fmt

%format Natural = "\hnat "
%format mempty = "\hmempty "
%format <> = "\hcmb "
\begin{code}

module Data.Dist where

import Numeric.Natural
import Data.Monoid
import Data.Monus
import Test.QuickCheck
import Control.DeepSeq

newtype Dist = Dist { runDist :: Natural }
  deriving stock (Eq, Ord)
  deriving (Show, Read, Num, Enum, Real, Integral, NFData) via Natural
  deriving (Semigroup, Monoid) via Sum Natural


instance Monus Dist where
  x |-| y
    | x <= y    = y - x
    | otherwise = x - y

instance Arbitrary Dist where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral
\end{code}
