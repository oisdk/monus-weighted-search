module MonusWeightedSearch.Examples.Dijkstra where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Writer

import Data.Monus.Dist
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty

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

shortestPaths :: Ord a => Graph a -> a -> [(NonEmpty a, Dist)]
shortestPaths g x =
  evalState (searchT (pathed (choices (\(x,w) -> tell w >> unique x) . g) =<< unique x)) Set.empty
{-# INLINE shortestPaths #-}

choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f = foldr ((<|>) . f) empty
{-# INLINE choices #-}

pathed :: MonadPlus m => (a -> m a) -> a -> m (NonEmpty a)
pathed f = star (\ ~(x :| xs) -> fmap (:|x:xs) (f x)) . (:| [])
{-# INLINE pathed #-}

star :: MonadPlus m => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)
{-# INLINE star #-}
