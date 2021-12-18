-- | 

module Control.Comonad.Heap.Pointed
  (Heap(..)
  ,Root
  ,pattern Root
  ,popMin
  ,singleton
  ,(<+>)
  ,dijkstra
  ,monusSort
  ,fromList)
  where

import qualified Control.Comonad.Heap as NonEmpty

import qualified Data.Set as Set

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic, Generic1 )
import Control.DeepSeq (NFData(..))

import Test.QuickCheck
import Data.Functor.Identity

import Data.List (unfoldr)

import Data.Monus
import Data.Monus.Dist
import Data.Monus.Max
import Data.Tuple (swap)
import Data.List.NonEmpty (nonEmpty)

type Root w = NonEmpty.Heap w

pattern Root :: w -> a -> [NonEmpty.HeapT w Identity a] -> NonEmpty.HeapT w Identity a
pattern Root w x xs = w NonEmpty.:< Identity (x NonEmpty.:> xs)
{-# COMPLETE Root #-}

data Heap w a
  = Leaf
  | Node {-# UNPACK #-} !(Root w a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data, Typeable, Generic, Generic1)

heap :: b -> (Root w a -> b) -> Heap w a -> b
heap b k = \case
  Leaf -> b
  Node xs -> k xs
{-# INLINE heap #-}

node :: Maybe (Root w a) -> Heap w a
node = maybe Leaf Node
{-# INLINE node #-}

root :: Heap w a -> Maybe (Root w a) 
root = heap Nothing Just
{-# INLINE root #-}

instance (NFData w, NFData a) => NFData (Heap w a) where
  rnf = heap () rnf

instance (Arbitrary w, Arbitrary a) => Arbitrary (Heap w a) where
  arbitrary = fmap node arbitrary
  shrink = map node . shrink . root

(<+>) :: Monus w => Heap w a -> Heap w a -> Heap w a
Leaf <+> ys = ys
xs <+> Leaf = xs
Node xs <+> Node ys = Node (xs NonEmpty.<+> ys)
{-# INLINE (<+>) #-}

popMin :: Monus w => Heap w a -> Maybe ((w, a), Heap w a)
popMin = fmap (fmap node . NonEmpty.popMin) . root
{-# INLINE popMin #-}

(<><) :: Semigroup w => w -> Heap w a -> Heap w a
w <>< Leaf = Leaf
w <>< Node (Root w' x xs) = Node (Root (w <> w') x xs)
{-# INLINE (<><) #-}

singleton :: w -> a -> Heap w a
singleton w x = Node (Root w x [])
{-# INLINE singleton #-}

fromList :: Monus w => [(w, a)] -> Heap w a
fromList = node . fmap NonEmpty.mergeHeaps . nonEmpty . map (uncurry NonEmpty.singleton)
{-# INLINE fromList #-}

-- | An implementation of Dijkstra's algorithm on 'Graph's.
dijkstra :: Ord a => Graph a -> Graph a
dijkstra g s = go Set.empty (singleton mempty s)
  where
    go s hp = case popMin hp of
      Nothing -> []
      Just ((w,x),xs)
        | Set.member x s -> go s xs
        | otherwise -> (x,w) : go (Set.insert x s) (xs <+> xs')
        where
          xs' = w <>< fromList (map swap (g x))

-- | Heapsort.
monusSort :: Ord a => [a] -> [a]
monusSort = map snd . unfoldr popMin . fromList . map (\x -> (In x, x))
