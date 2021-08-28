module MonusWeightedSearch.Sampling where

import Control.Monad.Heap
import Data.Monus.Prob
import Data.Ratio
import System.Random

sample :: Heap Prob a -> IO a
sample = go 1 . search where
  go r ((x,Prob px):xs) = do
    let f = r * px
    c <- randomRIO (1, toInteger (denominator f))
    if fromInteger c <= numerator f  then pure x else go (r / (1 - f)) xs
  go r [] = error "impossible"
