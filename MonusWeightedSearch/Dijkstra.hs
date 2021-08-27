module MonusWeightedSearch.Dijkstra (unique, dijkstra) where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Star
import Control.Monad.Writer

import Data.Monus.Dist
import Data.Set (Set)
import qualified Data.Set as Set

import Data.WeightedGraph

import Control.Monad.Heap

unique :: Ord a => a -> HeapT w (State (Set a)) a
unique x = do
  seen <- get
  guard (Set.notMember x seen)
  modify (Set.insert x)
  pure x
{-# INLINE unique #-}

dijkstra :: Ord a => Graph a -> a -> [(a, Dist)]
dijkstra g x =
  evalState (searchT (star (choices (\(x,w) -> tell w >> unique x) . g) =<< unique x)) Set.empty
{-# INLINE dijkstra #-}

choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f = foldr ((<|>) . f) empty
{-# INLINE choices #-}
