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
import Control.Comonad.Heap (pattern Root)

import qualified Data.Set as Set

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic, Generic1 )
import Control.DeepSeq (NFData(..))

import Control.Applicative
import Test.QuickCheck
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Data.List (unfoldr)

import Data.Monus
import Data.Monus.Dist
import Data.Monus.Max
import Data.Tuple (swap)
import Data.List.NonEmpty (nonEmpty)

type Root = NonEmpty.Heap

data Heap w a
  = Leaf
  | Node {-# UNPACK #-} !(Root w a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Data, Typeable, Generic, Generic1)

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

instance Bifunctor Heap where
  bimap f g = heap Leaf (Node . bimap f g)

instance Bifoldable Heap where
  bifold = heap mempty bifold
  
  bifoldMap f g = heap mempty (bifoldMap f g)

  bifoldr f g b = heap b (bifoldr f g b)
  bifoldl f g b = heap b (bifoldl f g b)

instance Bitraversable Heap where
  bitraverse f g Leaf = pure Leaf
  bitraverse f g (Node (Root w x xs)) = liftA3 (\w' x' xs' -> Node (Root w' x' xs')) (f w) (g x) (traverse (bitraverse f g) xs)

instance Arbitrary2 Heap where
  liftArbitrary2 ls rs = sized (\n -> frequency [(1, pure Leaf), (n, fmap Node (liftArbitrary2 ls rs))])
  liftShrink2 _ _ Leaf = []
  liftShrink2 ls rs (Node x) = Leaf : map Node (liftShrink2 ls rs x)
  
instance Arbitrary w => Arbitrary1 (Heap w) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
  
instance (Arbitrary w, Arbitrary a) => Arbitrary (Heap w a) where
  arbitrary = liftArbitrary2 arbitrary arbitrary
  shrink = liftShrink2 shrink shrink

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
fromList = node . fmap (NonEmpty.mergeHeaps . fmap (uncurry NonEmpty.singleton)) . nonEmpty
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
