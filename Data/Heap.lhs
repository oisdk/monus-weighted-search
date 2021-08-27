%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
\begin{code}
module Data.Heap where

import Data.Monus
import Data.WeightedGraph

import qualified Data.Set as Set

import Data.Dist
import Data.List (unfoldr)

import qualified Data.Tree.Binary.Leafy as Tree

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup


\end{code}
%<*root-def>
\begin{spec}
data Root = Root !a b [Root a b]
\end{spec}
%</root-def>
%<*heap-def>
\begin{spec}
data Heap a b = Leaf | Branch (Root a b)
\end{spec}
%</heap-def>
\begin{code}
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
    go x1 (x2 : []) = x1 <> x2
    go x1 (x2 : x3 : xs) = (x1 <> x2) <> go x3 xs
{-# INLINE mergeHeaps #-}

(<><) :: Monus a => a -> Heap a b -> Heap a b
x <>< Leaf = Leaf
x <>< Node y yv ys = Node (x <> y) yv ys
{-# INLINE (<><) #-}

minView :: Monus a => Heap a b -> Maybe ((a, b), Heap a b)
minView Leaf = Nothing
minView (Node x xv xs) = Just ((x, xv), x <>< mergeHeaps xs)
{-# INLINE minView #-}

singleton :: a -> b -> Heap a b
singleton x y = Node x y []
{-# INLINE singleton #-}

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

huffman :: Monus a => [(a, b)] -> Tree.Tree b
huffman = go . mergeHeaps . map (\(x,y) -> Node x (Tree.Leaf y) [])
  where
    go hp = case minView hp of
      Nothing -> error "huffman given empty list"
      Just ((xw, x), xs) -> case minView xs of
        Nothing -> x
        Just ((yw, y), ys) -> go (singleton (xw <> yw) (x Tree.:*: y) <> ys)

dsort :: [Dist] -> [Dist]
dsort = map fst . unfoldr minView . foldMap (\x -> singleton x ())
{-# INLINE dsort #-}
\end{code}
