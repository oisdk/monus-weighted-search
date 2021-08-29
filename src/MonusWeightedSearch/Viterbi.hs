module MonusWeightedSearch.Viterbi where

import Control.Monad.Heap
import Data.Monus.Prob
import Data.Foldable
import Control.Monad.Writer
import Data.Maybe

type Viterbi = Heap Prob

data Obs = Normal | Cold | Dizzy deriving (Show, Eq)
data States = Healthy | Fever deriving (Show, Eq)

start :: Viterbi States
start = asum [writer (Healthy, 0.6), writer (Fever, 0.4)]

trans :: States -> Viterbi States
trans Healthy = asum [writer (Healthy, 0.7), writer (Fever, 0.3)]
trans Fever   = asum [writer (Healthy, 0.4), writer (Fever, 0.6)]

emit :: States -> Viterbi Obs
emit Healthy = asum [writer (Normal, 0.5), writer (Cold, 0.4), writer (Dizzy, 0.1)]
emit Fever   = asum [writer (Normal, 0.1), writer (Cold, 0.3), writer (Dizzy, 0.6)]

iterateM :: Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateM n f = go n id
  where
    go 0 k xs = pure (k [])
    go n k xs = xs >>= \x -> go (n-1) (k . (x:)) (f x)

likely :: (Prob, [States])
likely = fromJust $ best $ do
  hidden <- iterateM 3 trans start
  obs <- traverse emit hidden
  guard ([Normal, Cold, Dizzy] == obs)
  return hidden