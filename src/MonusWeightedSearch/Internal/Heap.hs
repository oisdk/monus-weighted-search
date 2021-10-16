-- |
-- Module      : MonusWeightedSearch.Internal.Heap
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A reference implementation of a pairing heap, to compare to the heap monad.

module MonusWeightedSearch.Internal.Heap (Heap, Root(..),minView, singleton, dijkstra, monusSort) where

import Data.Monus
import Data.Monus.Dist
import Data.Monus.Max

import qualified Data.Set as Set

import Data.List (unfoldr)

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Semigroup

-- | A pairing heap.
--
-- This implementation does use a monus rather than just a standard ordered
-- key, but that does not change any of the algorithms really.
type Heap a b = Maybe (Root a b)

-- | A non-empty heap.
data Root a b = Node !a b [Root a b]
  deriving (Show, Functor, Foldable, Traversable)

instance Monus a => Semigroup (Root a b) where
  Node x xv xs <> Node y yv ys
    | x <= y    = Node x xv (Node (y |-| x) yv ys : xs)
    | otherwise = Node y yv (Node (x |-| y) xv xs : ys)
  {-# INLINE (<>) #-}

  sconcat (x :| []) = x
  sconcat (x1 :| x2 : []) = x1 <> x2
  sconcat (x1 :| x2 : x3 : xs) = (x1 <> x2) <> sconcat (x3 :| xs)
  {-# INLINABLE sconcat #-}

mergeHeaps :: Monus a => [Root a b] -> Heap a b
mergeHeaps = fmap sconcat . nonEmpty
{-# INLINE mergeHeaps #-}

(<><) :: Semigroup a => a -> Root a b -> Root a b
x <>< Node y yv ys = Node (x <> y) yv ys
{-# INLINE (<><) #-}

-- | /O(log n)/. Pop the minimum element and its key in the heap, and return it.
minViewR :: Monus a => Root a b -> ((a, b), Heap a b)
minViewR (Node x xv xs) = ((x, xv), fmap ((x <><) . sconcat) (nonEmpty xs))
{-# INLINE minViewR #-}

minView :: Monus a => Heap a b -> Maybe ((a, b), Heap a b)
minView = fmap minViewR
{-# INLINE minView #-}

-- | A singleton heap.
singleton :: a -> b -> Heap a b
singleton x y = Just (Node x y [])
{-# INLINE singleton #-}

-- | An implementation of Dijkstra's algorithm on 'Graph's.
dijkstra :: Ord a => Graph a -> Graph a
dijkstra g s = go Set.empty (singleton mempty s)
  where
    go s hp = case minView hp of
      Nothing -> []
      Just ((w,x),xs)
        | Set.member x s -> go s xs
        | otherwise -> (x,w) : go (Set.insert x s) (xs <> mergeHeaps (map f (g x)))
          where
            f (y, w') = Node (w <> w') y []
{-# INLINE dijkstra #-}

-- | Heapsort.
monusSort :: Ord a => [a] -> [a]
monusSort = map snd . unfoldr minView . foldMap (\x -> singleton (In x) x)
{-# INLINE monusSort #-}
