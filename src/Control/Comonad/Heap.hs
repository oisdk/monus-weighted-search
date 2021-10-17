-- | 

module Control.Comonad.Heap where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Monus.Dist
import Data.Monus.Max
import qualified Data.Set as Set

import Control.Comonad.Cofree.Class
import Control.Comonad
import Data.Monus
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, unfoldr, toList)
import MonusWeightedSearch.Internal.TestHelpers
import Test.QuickCheck
import Control.Applicative

data Heap w a
  = Root !w a [Heap w a]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Bifunctor Heap where
  bimap f g (Root w x xs) = Root (f w) (g x) (map (bimap f g) xs)

instance Bifoldable Heap where
  bifoldMap f g (Root w x xs) = f w <> g x <> foldMap (bifoldMap f g) xs
  bifoldr f g b (Root w x xs) = f w (g x (foldr (flip (bifoldr f g)) b xs))
  bifoldl f g b (Root w x xs) = foldl (bifoldl f g) (g (f b w) x) xs
  
instance Bitraversable Heap where
  bitraverse f g (Root w x xs) = liftA3 Root (f w) (g x) (traverse (bitraverse f g) xs)

instance Arbitrary2 Heap where
  liftArbitrary2 la ra = sized go
    where
      go n = Root <$> la <*> ra <*> (sumsTo (n-1) >>= traverse go)

  liftShrink2 ls rs (Root w x xs) =
    xs ++ [Root w' x' xs' | ((w', x'), xs') <- liftShrink2 (liftShrink2 ls rs) (liftShrink (liftShrink2 ls rs)) ((w, x), xs) ]

instance Arbitrary w => Arbitrary1 (Heap w) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
  
instance (Arbitrary w, Arbitrary a) => Arbitrary (Heap w a) where
  arbitrary = arbitrary2
  shrink = shrink2

(<+>) :: Monus w => Heap w a -> Heap w a -> Heap w a
Root xw x xs <+> Root yw y ys
  | xw <= yw  = Root xw x (Root (yw |-| xw) y ys : xs)
  | otherwise = Root yw y (Root (xw |-| yw) x xs : ys)

mergeHeaps :: Monus w => NonEmpty (Heap w a) -> Heap w a
mergeHeaps (x  :| [])           = x
mergeHeaps (x1 :| x2 : [])      = x1 <+> x2
mergeHeaps (x1 :| x2 : x3 : xs) = (x1 <+> x2) <+> mergeHeaps (x3 :| xs)

(<><) :: Semigroup w => w -> Heap w a -> Heap w a
(<><) w (Root ws x xs) = Root (w <> ws) x xs

popMin :: Monus w => Heap w a -> ((w, a), Maybe (Heap w a))
popMin (Root w x xs) = ((w, x), fmap mergeHeaps (nonEmpty xs))

singleton :: w -> a -> Heap w a
singleton w x = Root w x []

instance Comonad (Heap w) where
  extract (Root _ x _) = x
  {-# INLINE extract #-}
  duplicate xh@(Root w x xs) = Root w xh (map duplicate xs)
  {-# INLINE duplicate #-}
  extend f xh@(Root w _ xs) = Root w (f xh) (map (extend f) xs)
  {-# INLINE extend #-}

instance ComonadCofree [] (Heap w) where
  unwrap (Root w x xs) = xs
  {-# INLINE unwrap #-}

dijkstra :: Ord a => Graph a -> Graph a
dijkstra g s = go Set.empty (Just (singleton mempty s))
  where
    go s Nothing = []
    go s (Just hp) = case popMin hp of
      ((w,x),xs)
        | Set.member x s -> go s xs
        | otherwise -> (x,w) : go (Set.insert x s) (fmap mergeHeaps (nonEmpty (foldr (:) (map f (g x)) xs)))
          where
            f (y, w') = Root (w <> w') y []

-- | Heapsort.
monusSort :: Ord a => [a] -> [a]
monusSort =
  maybe
    []
    (toList . fmap snd . unfoldr popMin . mergeHeaps . fmap (\x -> singleton (In x) x)) . nonEmpty
{-# INLINE monusSort #-}
