%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"
%format :< = "\mathbin{\triangleleft} "
%format Set.empty = "\emptyset "
%format Set.member (x) (y) = x "\in " y
%format Set.insert (x) (y) = "\left\{ " x "\right\} \cup " y
%format PosRat = "\hposrat "
%format !-! = "\hmonus "
%format (frac (x) (y)) = "\frac{ " x "}{" y "}"
%format <> = "\hcmb "
%format mempty = "\hmempty "
%format (diff (x) (y)) = " \lvert " x - y " \rvert"
%format DispProb = "\hprob"
%format (Prob (x)) = x
\begin{code}
module Data.Prob where

import Control.Applicative
import Data.Monus
import Data.Ratio
import Numeric.Natural
import Test.QuickCheck

type PosRat = Ratio Natural


frac :: Prob -> Prob -> Prob
frac (Prob x) (Prob y) = Prob (x / y)


type DispProb = Prob

isMoreLikelyThan :: Prob -> Prob -> Bool
isMoreLikelyThan (Prob x) (Prob y) = x > y

\end{code}
%<*prob-ty>
\begin{code}
newtype Prob = Prob { runProb :: PosRat }
\end{code}
%</prob-ty>
\begin{code}
  deriving stock Eq
  deriving newtype Show

\end{code}
%<*ord-inst>
\begin{code}
instance Ord DispProb where
  x <= y =
    not (y `isMoreLikelyThan` x)
\end{code}
%</ord-inst>
\begin{code}
createProb :: PosRat -> Prob
createProb n = Prob n
\end{code}
%<*monoid-inst>
\begin{code}
instance Semigroup DispProb where
  Prob x <> Prob y = Prob (x * y)

instance Monoid DispProb where
  mempty = Prob 1
\end{code}
%</monoid-inst>
\begin{code}
instance Bounded Prob where
  minBound = Prob 1
  maxBound = Prob 0
\end{code}
%<*monus-inst>
\begin{code}
instance Monus DispProb where diff x y = case compare x y of LT -> frac y x; EQ -> 1; GT -> frac x y
\end{code}
%</monus-inst>
\begin{code}


instance Arbitrary Prob where
  arbitrary = liftA2 f arbitrary arbitrary
    where
      f (NonNegative n) (Positive d) = Prob (fromInteger n % fromInteger (n + d))

instance Num Prob where
  Prob x + Prob y = Prob (x + y)

  fromInteger = createProb . fromInteger

  (*) = (<>)

  abs = id

  signum (Prob 0) = Prob 0
  signum _ = Prob 1

  Prob x - Prob y = Prob (x - y)

instance Real Prob where
  toRational = toRational . runProb

instance Fractional Prob where
  fromRational = createProb . fromRational
  Prob x / Prob y = createProb (x / y)
\end{code}
