%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format <*> = "\hap "
%format <$> = "\hfmap "

%format _liftA2 = liftA2

\begin{code}
module Control.Applicative.Free (Ap(..), lower, zapWith) where

import Control.Applicative
\end{code}
%<*ap-def>
\begin{code}
data Ap f a where
  Pure  :: a -> Ap f a
  Lift  ::  (a -> b -> c) ->
            f a -> Ap f b -> Ap f c
\end{code}
%</ap-def>
\begin{code}
instance Functor (Ap f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Lift g x xs) = Lift (\a b -> f (g a b)) x xs
\end{code}
%<*applicative-free>
\begin{code}
instance Applicative (Ap f) where
  pure :: a -> Ap f a
  pure = Pure

  (<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b
  Pure x       <*> Pure y       = Pure (x y)
  Pure x       <*> Lift g y ys  = Lift (\y ys -> x (g y ys)) y ys
  Lift f x xs  <*> ys           = Lift (\x (y,z) -> f x y z) x (liftA2 (,) xs ys)
\end{code}
%</applicative-free>
%<*lower>
\begin{code}
lower :: Applicative f => Ap f a -> f a
lower (Pure x)       = pure x
lower (Lift f x xs)  = liftA2 f x (lower xs)
\end{code}
%</lower>
%<*zap>
\begin{code}
zapWith :: Applicative f => (a -> b -> c) -> Ap f a -> Ap f b -> Ap f c
zapWith (*) (Pure x)         ys               = fmap (x *) ys
zapWith (*) xs               (Pure y)         = fmap (* y) xs
zapWith (*) (Lift (+) x xs)  (Lift (^) y ys)  =
  Lift (\(x,y) (xs,ys) -> (x + xs) * (y ^ ys)) (liftA2 (,) x y) (zapWith (,) xs ys)
\end{code}
%</zap>
%<*liftA2>
\begin{code}
_liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
_liftA2 f xs ys = pure f <*> xs <*> ys
\end{code}
%</liftA2>
