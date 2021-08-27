%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format :< = "\mathbin{\triangleleft} "
%format ! = "\;!"

\begin{code}
module Data.Node where

import Data.Bifunctor

infixr 5 :<
\end{code}
%<*node-ty>
\begin{code}
data Node w a b = Leaf a | !w :< b
\end{code}
%</node-ty>
\begin{code}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Bifunctor (Node w) where
  bimap f g (Leaf x) = Leaf (f x)
  bimap f g (x :< xs) = x :< g xs

  first f (Leaf x) = Leaf (f x)
  first f (x :< xs) = x :< xs

  second f (Leaf x) = Leaf x
  second f (x :< xs) = x :< f xs

  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
\end{code}
