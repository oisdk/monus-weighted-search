%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format <> = "\hcmb "
%format ==> = "\implies "
%format period_ = ".\;"
%format <*> = "\hap"

\begin{code}
module Control.Applicative.Exp where

import Data.Bifunctor
import Control.Applicative
import Prelude hiding (abs)

\end{code}
%<*exp-def>
\begin{code}
newtype Exp f g a = Exp { runExp :: forall b. f (b) -> g (a, b) }
\end{code}
%</exp-def>
%<*endo-def>
\begin{code}
newtype Cayley f a = Cayley { runC :: forall b. f b -> f (a, b) }
\end{code}
%</endo-def>
\begin{code}
instance Functor f => Functor (Cayley f) where
  fmap f (Cayley k) = Cayley (fmap (first f) . k)
\end{code}
%<*app-inst>
\begin{code}
instance Functor f => Applicative (Cayley f) where
  pure x     = Cayley (fmap (x,))
  fs <*> xs  = Cayley (fmap (\(f,(x,xs)) -> (f x, xs)) . runC fs . runC xs)
\end{code}
%</app-inst>
%<*abs>
\begin{code}
abs :: Applicative f => f a -> Cayley f a
abs x = Cayley (liftA2 (,) x)
\end{code}
%</abs>
%<*rep>
\begin{code}
rep :: Applicative f => Cayley f a -> f a
rep x = fmap fst (runC x (pure ()))
\end{code}
%</rep>
