%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format :*: = "\times"
%format Empty = "\langle \rangle"
%format (Leaf (x)) = "\langle " x " \rangle"
%format mempty = "\hmempty "
%format <> = "\hcmb "

\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Data.Tree.Leafy where

import Control.Applicative
import Test.QuickCheck

\end{code}
%<*tree-def>
\begin{code}
data Tree a = Leaf a | Tree a :*: Tree a
\end{code}
%</tree-def>
\begin{code}
  deriving Functor
instance Foldable Tree where
\end{code}
%<*foldMap>
\begin{code}
  foldMap ::  Monoid m =>
              (a -> m) ->
              Tree a -> m
  foldMap f (Leaf x)     = f x
  foldMap f (xs :*: ys)  =
    foldMap f xs <> foldMap f ys
\end{code}
%</foldMap>
%<*dfs>
\begin{code}
dfs :: Tree a -> [a]
dfs = foldMap (\x -> [x])
\end{code}
%</dfs>
%<*uncons>
\begin{code}
uncons :: Tree a -> (a, Maybe (Tree a))
uncons (Leaf x)              = (x, Nothing)
uncons (Leaf x :*: xs)       = (x, Just xs)
uncons ((xs :*: ys) :*: zs)  = uncons (xs :*: (ys :*: zs))
\end{code}
%</uncons>
\begin{code}
instance Applicative Tree where
  pure = Leaf
  Leaf f <*> xs = fmap f xs
  (fl :*: fr) <*> xs = (fl <*> xs) :*: (fr <*> xs)

instance Monad Tree where
  Leaf x >>= f = f x
  (xs :*: ys) >>= f = (xs >>= f) :*: (ys >>= f)

instance Traversable Tree where
  traverse f (Leaf x) = fmap Leaf (f x)
  traverse f (xs :*: ys) = liftA2 (:*:) (traverse f xs) (traverse f ys)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go n
        | n <= 1 = fmap Leaf arbitrary
        | otherwise = do
          m <- choose (1, n - 1)
          liftA2 (:*:) (go m) (go (n-m))

  shrink (xs :*: ys) = xs : ys : map (uncurry (:*:)) (shrink (xs,ys))
  shrink (Leaf _) = []
\end{code}
