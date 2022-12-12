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
-- of that, see the module "MonusWeightedSearch.Examples.Parsing", where the
-- heap is used to implement a probabilistic parser.

module MonusWeightedSearch.Examples.Dijkstra where

import Prelude hiding (head)
import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Writer
import Control.Monad
import Data.Foldable

import Data.Monus.Dist
import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty (NonEmpty(..))

import Control.Monad.Heap

-- $setup
-- >>> import Prelude hiding (head)
-- >>> import Data.List.NonEmpty (head)

-- | The example graph from
-- <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm the Wikipedia article on Dijkstra's algorithm>.
--
-- <<https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif>>
graph :: Graph Int
graph 1 = [(2,7),(3,9),(6,14)]
graph 2 = [(3,10),(4,15)]
graph 3 = [(4,11), (6,2)]
graph 4 = [(5,6)]
graph 5 = []
graph 6 = [(5,9)]
graph _ = []

-- | @'unique' x@ checks that @x@ has not yet been seen in this branch of the
-- computation.
unique :: Ord a => a -> HeapT w (State (Set a)) a
unique x = do
  seen <- get
  guard (Set.notMember x seen)
  modify (Set.insert x)
  pure x
{-# INLINE unique #-}

-- | This is the Kleene star on the semiring of 'MonadPlus'. It is analagous to
-- the 'many' function on 'Alternative's.
star :: MonadPlus m => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)
{-# INLINE star #-}

-- | This is a version of 'star' which keeps track of the inputs it was given.
pathed :: MonadPlus m => (a -> m a) -> a -> m (NonEmpty a)
pathed f = star (\ ~(x :| xs) -> fmap (:|x:xs) (f x)) . (:| [])
{-# INLINE pathed #-}

-- | Dijkstra's algorithm. This function returns the length of the shortest path
-- from a given vertex to every vertex in the graph.
--
-- >>> dijkstra graph 1
-- [(1,0),(2,7),(3,9),(6,11),(5,20),(4,20)]
--
-- A version which actually produces the paths is 'shortestPaths'
dijkstra :: Ord a => Graph a -> a -> [(a, Dist)]
dijkstra g x =
  evalState (searchT (star (asum . map (\(x,w) -> tell w >> unique x) . g) =<< unique x)) Set.empty
{-# INLINE dijkstra #-}

-- | Dijkstra's algorithm, which produces a path.
--
-- The only difference between this function and 'shortestPaths' is that this
-- uses 'pathed' rather than 'star'.
--
-- The following finds the shortest path from vertex 1 to 5:
--
-- >>> filter ((5==) . head . fst) (shortestPaths graph 1)
-- [(5 :| [6,3,1],20)]
--
-- And it is indeed @[1,3,6,5]@. (it's returned in reverse)
shortestPaths :: Ord a => Graph a -> a -> [(NonEmpty a, Dist)]
shortestPaths g x =
  evalState (searchT (pathed (asum . map (\(x,w) -> tell w >> unique x) . g) =<< unique x)) Set.empty
{-# INLINE shortestPaths #-}


