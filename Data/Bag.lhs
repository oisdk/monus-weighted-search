%include polycode.fmt
%include forall.fmt
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"

%format Bag (a) = "\lbagcon" a "\rbagcon"
%format <|> = "\halt "
%format <*> = "\hap "
%format <> = "\hcmb "

\begin{code}
module Data.Bag (Bag(Empty),pattern Sing,fromList) where

import Data.Tree.Leafy

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Foldable (toList)
import Data.Function (on)
import Control.Monad
import Control.Applicative

import Test.QuickCheck

data Bag a = Empty | Some (Tree a) deriving (Functor, Foldable, Traversable)

pattern Sing :: a -> Bag a
pattern Sing x = Some (Leaf x)

frequencies :: Ord a => Bag a -> Map a Word
frequencies = Map.fromListWith (+) . map (flip (,) 1) . toList

instance Semigroup (Bag a) where
  Empty <> ys = ys
  Some xs <> ys = Some (case ys of
                          Empty -> xs
                          Some ys' -> xs :*: ys')
instance Monoid (Bag a) where mempty = Empty

instance Ord a => Eq (Bag a) where (==) = (==) `on` frequencies
instance Ord a => Ord (Bag a) where compare = compare `on` frequencies

instance (Ord a, Show a) => Show (Bag a) where
  showsPrec _ xs s | null xs = "{}" ++ s
  showsPrec _ xs s = '{' : drop 2 (Map.foldrWithKey f ('}' : s) (frequencies xs))
    where
      f x n xs = showString ", " . shows x . showString ": " . shows n $ xs

fromList :: [a] -> Bag a
fromList = foldMap Sing

instance Applicative Bag where
  pure = Sing
  Empty  <*> _ = Empty
  Some _ <*> Empty = Empty
  Some fs <*> Some xs = Some (fs <*> xs)

instance Monad Bag where
  (>>=) = flip foldMap

instance Alternative Bag where
  (<|>) = (<>)
  empty = Empty

instance MonadPlus Bag

instance Arbitrary a => Arbitrary (Bag a) where
  arbitrary = sized go
    where
      go n = frequency [(1, pure Empty), (n, fmap Some arbitrary)]

  shrink Empty = []
  shrink (Some xs) = Empty : map Some (shrink xs)
\end{code}
%<*quot>
\begin{spec}
type Bag a = [a] / Perm
\end{spec}
%</quot>
