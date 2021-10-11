-- |
-- Module      : MonusWeightedSearch.Examples.Sort
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Sorting using the 'Heap' monad.
--
-- The 'Heap' monad can function like a normal heap (although it does need a
-- 'Monus' instead of just any ordered key), and as such it can implement a
-- normal sorting algorithm.

module MonusWeightedSearch.Examples.Sort where

import Data.Monus.Max
import Control.Monad.Heap

-- $setup
-- >>> default (Word)

-- | /O(n log n)/. Heapsort.
--
-- >>> monusSort [5,1,2,3,1,6,3,2,5,7]
-- [1,1,2,2,3,3,5,5,6,7]
monusSort :: Ord a => [a] -> [a]
monusSort = map fst . search . fromList . map (\x -> (x, In x)) 
{-# INLINE monusSort #-}
