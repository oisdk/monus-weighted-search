-- |
-- Module      : MonusWeightedSearch.Examples.Dijkstra
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- An implementation of Dijkstra's algorithm, using the 'HeapT' monad.
--
-- This is taken from section 6.1.3 of the paper
--
-- * Donnacha Oisín Kidney and Nicolas Wu. 2021. /Algebras for weighted search/.
--   Proc. ACM Program. Lang. 5, ICFP, Article 72 (August 2021), 30 pages.
--   DOI:<https://doi.org/10.1145/3473577>
--
-- This is a pretty simple implementation of the algorithm, defined monadically,
-- but it retains the time complexity of a standard purely functional
-- implementation.
--
-- We use the state monad here to avoid searching from the same node more than
-- once (which would lead to an infinite loop). Different algorithms use
-- different permutations of the monad transformers: for Dijkstra's algorithm,
-- we use @'HeapT' w ('State' ('Set' a)) a@, i.e. the 'HeapT' is outside of the
-- 'State'. This means that each branch of the search proceeds with a different
-- state; if we switch the order (to @'StateT' s ('Heap' w) a@, for example), we
-- get "global" state, which has the semantics of a /parser/. For an example
-- of that, see the module "MonusWeightedSearch.Examples.Pairsing", where the
-- heap is used to implement a probabilistic parser.

module MonusWeightedSearch.Examples.Dijkstra where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Writer

import Data.Monus.Dist
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty

import Control.Monad.Heap

-- | @'unique' x@ checks that @x@ has not yet been seen in this branch of the
-- computation.
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
