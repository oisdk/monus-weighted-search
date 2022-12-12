-- |
-- Module      : MonusWeightedSearch.Examples.SubsetSum
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- An implementation of shortest subset sum (the Inclusion-Exclusion method)
-- using the 'Heap' monad.

module MonusWeightedSearch.Examples.SubsetSum where

import Control.Monad.Heap
import Data.Monus.Dist
import Control.Monad.Writer
import Data.Maybe
import Control.Monad (filterM, guard)

-- | A weight for the inclusion or exclusion of an element.
--
-- This lets us weight the computation by number of elements included, and
-- therefore optimise for the fewest.
inclusion :: Monad m => HeapT Dist m Bool
inclusion = fromList [(False,0),(True,1)]

-- | @'shortest' n xs@ returns the shortes subset of @xs@ which sums to @n@.
--
-- >>> shortest 5 [10,-4,3,11,6,12,1]
-- [-4,3,6]
shortest ::  Int -> [Int] -> [Int]
shortest t xs = snd . fromJust . best $ do
  subset <- filterM (const inclusion) xs
  guard (sum subset == t)
  pure subset
