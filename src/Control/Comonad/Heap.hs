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

module Control.Comonad.Heap
  (Heap(..)
  ,popMin
  ,(<+>)
  ,(<><)
  ,mergeHeaps
  ,singleton)
  where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Bifoldable ( Bifoldable(..) )
import Data.Bitraversable ( Bitraversable(..) )
import Control.Comonad.Cofree.Class ( ComonadCofree(..) )
import Control.Comonad ( Comonad(..) )
import Data.Monus ( Monus(..) )
import MonusWeightedSearch.Internal.TestHelpers ( sumsTo )
import Test.QuickCheck
    ( shrink2,
      arbitrary2,
      sized,
      Arbitrary(..),
      Arbitrary1(..),
      Arbitrary2(..) )
import Control.Applicative ( liftA3 )
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData(..), NFData1(..), NFData2(..))
import Control.Monad (ap)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor.Classes
    ( Eq1(..),
      Ord1(..),
      Read1(liftReadPrec),
      Show1(liftShowsPrec),
      Eq2(..),
      Ord2(..),
      Read2(liftReadListPrec2, liftReadPrec2),
      Show2(..) )
import Text.Read
    ( Read(readListPrec, readPrec),
      Lexeme(Ident),
      lexP,
      parens,
      prec,
      step )

data Heap w a
  = Root !w a [Heap w a]
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Data, Typeable, Generic, Generic1)

instance Eq2 Heap where
  liftEq2 e1 e2 (Root wx x xs) (Root wy y ys) = e1 wx wy && e2 x y && liftEq (liftEq2 e1 e2) xs ys
  
instance Ord2 Heap where
  liftCompare2 c1 c2 (Root wx x xs) (Root wy y ys) =
    c1 wx wy <> c2 x y <> liftCompare (liftCompare2 c1 c2) xs ys

instance Show2 Heap where
  liftShowsPrec2 s1 sl1 s2 sl2 = go where
    go d (Root w x xs) =
      showParen (d > 10) $ showString "Root " . s1 11 w . showString " " . s2 11 x . showString " " . liftShowsPrec go (liftShowList2 s1 sl1 s2 sl2) 11 xs

instance Read2 Heap where
  liftReadPrec2 r1 rl1 r2 rl2 = parens $ prec 10 $ do
    Ident "Root" <- lexP
    w <- step r1
    x <- step r2
    xs <- step (liftReadListPrec2 r1 rl1 r2 rl2)
    return (Root w x xs)

instance Eq w => Eq1 (Heap w) where liftEq = liftEq2 (==)
instance Ord w => Ord1 (Heap w) where liftCompare = liftCompare2 compare
instance Show w => Show1 (Heap w) where liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Read w => Read1 (Heap w) where liftReadPrec = liftReadPrec2 readPrec readListPrec

instance (NFData w, NFData a) => NFData (Heap w a) where
  rnf (Root w x xs) = rnf w `seq` rnf x `seq` rnf xs
  {-# INLINE rnf #-}

instance NFData2 Heap where
  liftRnf2 r1 r2 (Root w x xs) = r1 w `seq` r2 x `seq` liftRnf (liftRnf2 r1 r2) xs
  {-# INLINE liftRnf2 #-}
  
instance NFData w => NFData1 (Heap w) where
  liftRnf = liftRnf2 rnf
  {-# INLINE liftRnf #-}

instance Bifunctor Heap where
  bimap f g (Root w x xs) = Root (f w) (g x) (map (bimap f g) xs)
  {-# INLINE bimap #-}

instance Bifoldable Heap where
  bifold (Root w x xs) = w <> x <> foldMap bifold xs
  bifoldMap f g (Root w x xs) = f w <> g x <> foldMap (bifoldMap f g) xs
  bifoldr f g b (Root w x xs) = f w (g x (foldr (flip (bifoldr f g)) b xs))
  bifoldl f g b (Root w x xs) = foldl (bifoldl f g) (g (f b w) x) xs
  
instance Bitraversable Heap where
  bitraverse f g (Root w x xs) = liftA3 Root (f w) (g x) (traverse (bitraverse f g) xs)

instance Arbitrary2 Heap where
  liftArbitrary2 la ra = sized go
    where
      go n = liftA3 Root la ra (sumsTo (n-1) >>= traverse go)

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
{-# INLINE (<+>) #-}

mergeHeaps :: Monus w => NonEmpty (Heap w a) -> Heap w a
mergeHeaps (x  :| [])           = x
mergeHeaps (x1 :| x2 : [])      = x1 <+> x2
mergeHeaps (x1 :| x2 : x3 : xs) = (x1 <+> x2) <+> mergeHeaps (x3 :| xs)
{-# INLINE mergeHeaps #-}

(<><) :: Semigroup w => w -> Heap w a -> Heap w a
(<><) w (Root ws x xs) = Root (w <> ws) x xs
{-# INLINE (<><) #-}

popMin :: Monus w => Heap w a -> ((w, a), Maybe (Heap w a))
popMin (Root w x xs) = ((w, x), fmap ((w <><) . mergeHeaps) (nonEmpty xs))
{-# INLINE popMin #-}

singleton :: w -> a -> Heap w a
singleton w x = Root w x []
{-# INLINE singleton #-}

instance Monoid w => Applicative (Heap w) where
  pure x = Root mempty x []
  {-# INLINE pure #-}
  (<*>) = ap

instance Monoid w => Monad (Heap w) where
  Root w1 x xs >>= k = case k x of
    Root w2 y ys -> Root (w1 <> w2) y (ys ++ map (>>= k) xs)
  {-# INLINE (>>=) #-}

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
