%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format <> = "\hcmb "
%format ==> = "\implies "
%format . = ".\;"

\begin{code}
module Data.Functor.Day.LiftA2 where

\end{code}
%<*day-def>
\begin{code}
data Day f g a = forall x y. Day (x -> y -> a) (f x) (g y)
\end{code}
%</day-def>
\begin{code}
instance Functor g => Functor (Day f g) where
  fmap f (Day c xs ys) = Day ((f.) . c) xs ys
\end{code}
