module MonusWeightedSearch.Sort where

import Data.Monus
import Control.Monad.Heap
import Control.Monad.Writer
import Data.Foldable

monusSort :: Monus m => [m] -> [m]
monusSort = map snd . search . asum . map tell
{-# INLINE monusSort #-}
