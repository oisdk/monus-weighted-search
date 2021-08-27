%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format (diff (x) (y)) = "\lvert" x "-" y "\rvert"
%format HideNat = "\Nat"

\begin{code}
module Data.Monus (Monus(..)) where

import Numeric.Natural
import Data.Monoid
\end{code}
%<*monus-class>
\begin{spec}
class (Ord a, Monoid a) => Monus a where diff :: a -> a -> a
\end{spec}
%</monus-class>
\begin{code}
infixl 6 |-|
\end{code}
\begin{code}
class (Ord a, Monoid a) => Monus a where
  diff :: a -> a -> a
\end{code}
%if False
\begin{code}
  (|-|) :: a -> a -> a
  {-# MINIMAL (|-|) | diff #-}
  diff = (|-|)
  (|-|) = diff
\end{code}
%endif

\begin{code}
newtype HideNat
  = HideNat Natural
  deriving stock (Eq, Ord)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via (Sum Natural)

\end{code}
%<*monus-nat>
\begin{code}
instance Monus HideNat where diff x y = if x <= y then y - x else x - y
\end{code}
%</monus-nat>
\begin{code}
instance Monus () where
  diff _ _ = ()
\end{code}
