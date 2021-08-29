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

import Data.Monus
import Control.Monad.Heap

monusSort :: Monus m => [m] -> [m]
monusSort = map snd . search . fromList . map ((),) 
{-# INLINE monusSort #-}
