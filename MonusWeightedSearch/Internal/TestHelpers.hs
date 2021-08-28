{-# OPTIONS_GHC -Wno-orphans #-}

module MonusWeightedSearch.Internal.TestHelpers where

import Test.QuickCheck
import Numeric.Natural
import System.Random

-- | @'sumsTo' n@ generates a list that sums to @n@.
--
-- prop> n >= 0 ==> forAll (sumsTo n) (\xs -> sum xs === n)
sumsTo :: Int -> Gen [Int]
sumsTo n = go [] n >>= shuffle
  where
    go ks n
      | n <= 0 = pure ks
      | otherwise = do
          m <- choose (1, n)
          go (m : ks) (n-m)

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = map fromInteger . filter (0<=) . shrink . toInteger

-- | @'percentageChance' n@ is 'True' @n@% of the time, and 'False' the rest
-- of the time.
percentageChance :: Word -> Gen Bool
percentageChance n = fmap (n>) (choose (0,99))

-- | @'percentageChanceIO' n@ is 'True' @n@% of the time, and 'False' the rest
-- of the time.
percentageChanceIO :: Word -> IO Bool
percentageChanceIO n = fmap (n>) (randomRIO (0,99))
{-# INLINE percentageChanceIO #-}
