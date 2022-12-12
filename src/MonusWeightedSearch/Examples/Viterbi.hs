-- |
-- Module      : MonusWeightedSearch.Examples.Viterbi
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- An implementation of the Viterbi algorithm using the 'Heap' monad.
--
-- This algorithm follows almost exactly the
-- <https://en.wikipedia.org/wiki/Viterbi_algorithm#Example example given on Wikipedia>.
--
-- This actually implements the /lazy/ Viterbi algorithm, since the heap
-- prioritises likely results.

module MonusWeightedSearch.Examples.Viterbi where

import Control.Monad.Heap
import Data.Monus.Prob
import Control.Monad.Writer
import Control.Monad (guard)
import Data.Maybe

-- $setup
-- >>> import Data.Bifunctor (first)
-- >>> :set -XTypeApplications

-- | A heap of probabilities; similar to a probability monad, but prioritises
-- likely outcomes.
type Viterbi = Heap Prob

-- | The possible observations.
data Obs = Normal | Cold | Dizzy deriving (Show, Eq)

-- | The possible hidden states.
data States = Healthy | Fever deriving (Show, Eq)

-- | Then initial states (i.e. the estimated fever rate in the population).
start :: Viterbi States
start = fromList [(Healthy, 0.6), (Fever, 0.4)]

-- | The transition function: how likely is a healthy person to be healthy on
-- the following day? How likely is someone with a fever today to have one
-- tomorrow?
trans :: States -> Viterbi States
trans Healthy = fromList [(Healthy, 0.7), (Fever, 0.3)]
trans Fever   = fromList [(Healthy, 0.4), (Fever, 0.6)]

-- | Given the hidden state, what is the likelihood of the various observations.
emit :: States -> Viterbi Obs
emit Healthy = fromList [(Normal, 0.5), (Cold, 0.4), (Dizzy, 0.1)]
emit Fever   = fromList [(Normal, 0.1), (Cold, 0.3), (Dizzy, 0.6)]

-- | @'iterateM' n f x@ applies @f@ to @x@ @n@ times, collecting the results.
iterateM :: Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateM n f = go n id
  where
    go 0 k xs = pure (k [])
    go n k xs = xs >>= \x -> go (n-1) (k . (x:)) (f x)

-- | Given a sequence of observations, what is the most likely sequence of
-- hidden states?
--
-- For instance, if you observe normal, then cold, then dizzy, the underlying
-- states are most likely to be healthy, then healthy, then fever, with
-- probability 0.01512.
--
-- >>> first (realToFrac @_ @Double) (likely [Normal,Cold,Dizzy])
-- (1.512e-2,[Healthy,Healthy,Fever])
likely :: [Obs] -> (Prob, [States])
likely obs = fromJust $ best $ do
  hidden <- iterateM (length obs) trans start
  pobs <- traverse emit hidden
  guard (obs == pobs)
  return hidden
