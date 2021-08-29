-- |
-- Module      : MonusWeightedSearch.Internal.Heap
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A reference implementation of a pairing heap, to compare to the heap monad.

module MonusWeightedSearch.Internal.Heap (Heap(..),minView, singleton, dijkstra, monusSort) where

import Data.Monus
import MonusWeightedSearch.WeightedGraph

import qualified Data.Set as Set

import Data.Monus.Dist
import Data.List (unfoldr)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup

-- | A pairing heap.
--
-- This implementation does use a monus rather than just a standard ordered
-- key, but that does not change any of the algorithms really.
data Heap a b
  = Leaf
  | Node !a b [Heap a b]
  deriving (Show, Functor, Foldable, Traversable)

instance Monus a => Semigroup (Heap a b) where
  Leaf <> ys = ys
  xs <> Leaf = xs
  Node x xv xs <> Node y yv ys
    | x <= y    = Node x xv (Node (y |-| x) yv ys : xs)
    | otherwise = Node y yv (Node (x |-| y) xv xs : ys)
  {-# INLINE (<>) #-}

  sconcat (x :| []) = x
  sconcat (x1 :| [x2]) = x1 <> x2
  sconcat (x1 :| x2 : x3 : xs) = (x1 <> x2) <> sconcat (x3 :| xs)
  {-# INLINABLE sconcat #-}

instance Monus a => Monoid (Heap a b) where
  mempty = Leaf
  {-# INLINE mempty #-}

  mconcat []     = Leaf
  mconcat (x:xs) = sconcat (x :| xs)
  {-# INLINE mconcat #-}

mergeHeaps :: Monus a => [Heap a b] -> Heap a b
mergeHeaps [] = Leaf
mergeHeaps (x : xs) = go x xs
  where
    go x [] = x
    go x1 [x2] = x1 <> x2
    go x1 (x2 : x3 : xs) = (x1 <> x2) <> go x3 xs
{-# INLINE mergeHeaps #-}

(<><) :: Monus a => a -> Heap a b -> Heap a b
x <>< Leaf = Leaf
x <>< Node y yv ys = Node (x <> y) yv ys
{-# INLINE (<><) #-}

-- | /O(log n)/. Pop the minimum element and its key in the heap, and return it.
minView :: Monus a => Heap a b -> Maybe ((a, b), Heap a b)
minView Leaf = Nothing
minView (Node x xv xs) = Just ((x, xv), x <>< mergeHeaps xs)
{-# INLINE minView #-}

-- | A singleton heap.
singleton :: a -> b -> Heap a b
singleton x y = Node x y []
{-# INLINE singleton #-}

-- | An implementation of Dijkstra's algorithm on 'Graphs'.
dijkstra :: Ord a => Graph a -> Graph a
dijkstra g s = go Set.empty (Node mempty s [])
  where
    go s hp = case minView hp of
      Nothing -> []
      Just ((w,x),xs)
        | Set.member x s -> go s xs
        | otherwise -> (x,w) : go (Set.insert x s) (xs <> mergeHeaps (map f (g x)))
          where
            f (y, w') = Node (w <> w') y []
{-# INLINE dijkstra #-}

-- | Sort a list of 'Dist'.
monusSort :: [Dist] -> [Dist]
monusSort = map fst . unfoldr minView . foldMap (`singleton` ())
{-# INLINE monusSort #-}
