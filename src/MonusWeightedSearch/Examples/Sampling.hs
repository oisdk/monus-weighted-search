-- |
-- Module      : MonusWeightedSearch.Examples.Sampling
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Random sampling from the 'Heap' monad.
--
-- The 'Heap' monad can function as a probability monad, and it implements an
-- efficient sampling algorithm, based on reservoir sampling.

module MonusWeightedSearch.Examples.Sampling where

import Control.Monad.Heap
import Data.Monus.Prob
import Data.Ratio
import System.Random (randomRIO)

-- | @'withChance' p@ returns 'True' @p@ percent of the time.
withChance :: Integral a => Ratio a -> IO Bool
withChance f = fmap (toInteger (numerator f) >=) (randomRIO (1, toInteger (denominator f)))

-- | Sample a single value from the heap.
sample :: Heap Prob a -> IO a
sample hp = foldr f (error "impossible") (search hp) 1 where
  f (x,Prob px) k r = do
    let f = r * px
    c <- withChance f
    if c then pure x else k (r / (1 - f))
