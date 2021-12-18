--------------------------------------------------------------------------------
-- |
-- Module      : Control.Comonad.Heap
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- The Heap comonad: a comonad for efficient weighted search.
--
-- This module provides the 'Heap' *comonad*.
--------------------------------------------------------------------------------

{-# LANGUAGE UndecidableInstances #-}

module Control.Comonad.Heap
  (HeapT(..), RootF(..)
  ,Heap
  ,popMinT,popMin
  ,(<+>)
  ,(<><)
  ,mergeHeaps
  ,singleton)
  where

import Data.Bifunctor
import Data.Bifoldable ( Bifoldable(..) )
import Data.Bitraversable ( Bitraversable(..) )
import MonusWeightedSearch.Internal.TestHelpers ( sumsTo )
import Test.QuickCheck
import Control.Applicative ( liftA2 )
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData(..))
import Data.Monus
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor.Identity
import Control.Monad (ap, join)
import Control.Comonad
import Control.Comonad.Cofree.Class
import Data.Foldable (Foldable(foldl', foldr'))

import Control.Monad.State.Strict

data HeapT w m a
  = !w :< m (RootF a (HeapT w m a))
  deriving (Typeable, Generic)

type Heap w = HeapT w Identity

deriving instance (Show a, Show w, forall x. Show x => Show (m x)) => Show (HeapT w m a)
deriving instance (Eq a, Eq w, forall x. Eq x => Eq (m x)) => Eq (HeapT w m a)
deriving instance ( Ord w, Ord a
                  , forall x. Ord x => Ord (m x)
                  , Eq (HeapT w m a)
                  ) => Ord (HeapT w m a)

deriving instance (Data w, Data a, forall x. Data x => Data (m x), Typeable m) => Data (HeapT w m a)

data RootF a b
  = a :> [b]
  deriving (Functor, Foldable, Traversable, Typeable, Generic, Eq, Ord, Show, Read, Generic1, Data)

instance Bifunctor RootF where
  bimap f g (x :> y) = f x :> map g y
  {-# INLINE bimap #-}
  first f (x :> y) = f x :> y
  {-# INLINE first #-}
  second f (x :> y) = x :> map f y
  {-# INLINE second #-}

instance Bifoldable RootF where
  bifoldr f g b (x :> xs) = f x (foldr g b xs)
  bifoldMap f g (x :> xs) = f x <> foldMap g xs
  bifoldl f g b (x :> xs) = foldl g (f b x) xs
  {-# INLINE bifoldr #-}
  {-# INLINE bifoldMap #-}
  {-# INLINE bifoldl #-}

instance Bitraversable RootF where
  bitraverse f g (x :> xs) = liftA2 (:>) (f x) (traverse g xs)
  {-# INLINE bitraverse #-}

instance (NFData a, NFData b) => NFData (RootF a b) where
  rnf (x :> xs) = rnf x `seq` rnf xs
  {-# INLINE rnf #-}
  
instance (NFData w, NFData a, forall x. NFData x => NFData (m x)) => NFData (HeapT w m a) where
  rnf (x :< xs) = rnf x `seq` rnf xs
  {-# INLINE rnf #-}

instance Arbitrary2 RootF where
  liftArbitrary2 la ra = sized (\n -> liftA2 (:>) la (sumsTo (n-1) >>= traverse (`resize` ra)))
  liftShrink2 ls rs (x :> xs) = map (uncurry (:>)) (liftShrink2 ls (liftShrink rs) (x, xs))
  
instance Arbitrary a => Arbitrary1 (RootF a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (RootF a b) where
  arbitrary = liftArbitrary arbitrary
  shrink = liftShrink shrink

instance (Arbitrary w, Arbitrary1 m) => Arbitrary1 (HeapT w m) where
  liftArbitrary ra = sized go
    where
      go n = liftA2 (:<) arbitrary (resize (max 0 (n-1)) (liftArbitrary (liftA2 (:>) ra (sumsTo (n-2) >>= traverse go))))

  liftShrink rs (w :< xs) =
    map (uncurry (:<)) (liftShrink2 shrink (liftShrink (map (uncurry (:>)) . liftShrink2 rs (liftShrink (liftShrink rs)) . toPair)) (w, xs))
      where
        toPair (x :> xs) = (x, xs)

instance (Arbitrary w, Arbitrary a, forall x. Arbitrary x => Arbitrary (m x)) => Arbitrary (HeapT w m a) where
  arbitrary = sized go
    where
      go n = liftA2 (:<) arbitrary (resize (max 0 (n-1)) arbitrary)
  
instance Functor m => Functor (HeapT w m) where
  fmap f (w :< xs) = w :< fmap (bimap f (fmap f)) xs
  {-# INLINE fmap #-}

instance Foldable m => Foldable (HeapT w m) where
  foldr f b (_ :< xs) = foldr (\(y :> ys) z -> f y (foldr (flip (foldr f)) z ys)) b xs
  foldMap f (_ :< xs) = foldMap (\(y :> ys) -> f y <> foldMap (foldMap f) ys) xs
  foldl f b (_ :< xs) = foldl (\z (y :> ys) -> foldl (foldl f) (f z y) ys) b xs
  
  foldl' f !b (_ :< xs) = foldl' (\ !z (y :> ys) -> case f z y of !z' -> foldl' (foldl' f) z' ys) b xs
  foldr' f !b (_ :< xs) = foldr' (\(y :> ys) !z -> f y $! foldr (flip (foldr f)) z ys) b xs

instance Traversable m => Traversable (HeapT w m) where
  traverse f (w :< xs) = fmap (w :<) (traverse (bitraverse f (traverse f)) xs)

(<+>) :: (Monus w, Functor m) => HeapT w m a -> HeapT w m a -> HeapT w m a
(xw :< xh) <+> (yw :< yh)
  | xw <= yw  = xw :< fmap (\(x :> xs) -> x :> (((yw |-| xw) :< yh) : xs)) xh
  | otherwise = yw :< fmap (\(y :> ys) -> y :> (((xw |-| yw) :< xh) : ys)) yh
{-# INLINE (<+>) #-}

mergeHeaps :: (Monus w, Functor m) => NonEmpty (HeapT w m a) -> HeapT w m a
mergeHeaps (x  :| [])           = x
mergeHeaps (x1 :| x2 : [])      = x1 <+> x2
mergeHeaps (x1 :| x2 : x3 : xs) = (x1 <+> x2) <+> mergeHeaps (x3 :| xs)
{-# INLINE mergeHeaps #-}

(<><) :: Semigroup w => w -> HeapT w m a -> HeapT w m a
(<><) w (ws :< xs) = (w <> ws) :< xs
{-# INLINE (<><) #-}

popMinT :: (Monus w, Functor m) => HeapT w m a -> (w,  m (a, Maybe (HeapT w m a)))
popMinT (w :< xh) = (w,  fmap (\(x :> xs) -> (x, fmap ((w <><) . mergeHeaps) (nonEmpty xs))) xh)
{-# INLINE popMinT #-}

popMin :: Monus w => Heap w a -> ((w, a), Maybe (Heap w a))
popMin xs = case popMinT xs of
  (w, Identity (x, xs)) -> ((w, x), xs)
{-# INLINE popMin #-}

singleton :: Applicative m => w -> a -> HeapT w m a
singleton w x = w :< pure (x :> [])
{-# INLINE singleton #-}

instance (Monoid w, Monad m, Traversable m) => Applicative (HeapT w m) where
  pure x = mempty :< pure (x :> [])
  {-# INLINE pure #-}
  (<*>) = ap

instance (Monoid w, Monad m, Traversable m) => Monad (HeapT w m) where
  (w1 :< xs) >>= k = w2 :< join ys
    where
      (ys, w2) = runState (traverse act xs) w1
      act (x :> xs) = do
        let w :< ys = k x
        modify' (<> w)
        return (fmap (\(z :> zs) -> z :> (zs ++ map (>>= k) xs)) ys)
  {-# INLINE (>>=) #-}

instance Comonad m => Comonad (HeapT w m) where
  extract (_ :< xs) = case extract xs of
    x :> _ -> x
  {-# INLINE extract #-}

  extend f xh@(w :< xc) = w :< extend (\yc -> f (w :< yc) :> fmap (extend f) (let _ :> ys = extract yc in ys)) xc
  {-# INLINE extend #-}

instance Comonad m => ComonadCofree [] (HeapT w m) where
  unwrap (w :< xs) = let _ :> ys = extract xs in ys
  {-# INLINE unwrap #-}
