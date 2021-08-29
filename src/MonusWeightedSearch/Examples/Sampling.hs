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
import System.Random

-- | Sample a single value from the heap.
sample :: Heap Prob a -> IO a
sample = go 1 . search where
  go r ((x,Prob px):xs) = do
    let f = r * px
    c <- randomRIO (1, toInteger (denominator f))
    if fromInteger c <= numerator f  then pure x else go (r / (1 - f)) xs
  go r [] = error "impossible"
