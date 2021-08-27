%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"
%format (Frac (x) (y)) = "\frac{ " x "}{" y "}"
\begin{code}

module Control.Monad.Sampling where

import Data.Prob
import Control.Monad.Heap
import System.Random
import Data.Ratio
import Control.Arrow ((&&&))
import Control.Monad.Writer
import Data.Foldable

num, den :: Prob -> Int
num = fromEnum . numerator . runProb
den = fromEnum . denominator . runProb

choose :: [a] -> IO a
choose xs = fmap (xs !!) (randomRIO (0, length xs - 1))

pattern Frac :: Int -> Int -> Prob
pattern Frac n d <- (num &&& den -> (n, d))
  where Frac n d = Prob (toEnum n % toEnum d)
{-# COMPLETE Frac #-}
\end{code}
%<*sample>
\begin{code}
sample :: Heap Prob a -> IO a
sample = go 1 . search where
  go r ((x,px):xs) = do
    let Frac n d = r * px
    c <- randomRIO (1, d)
    if c <= n  then pure x else go (r / (1 - Frac n d)) xs
\end{code}
%</sample>
\begin{code}
  go _ [] = error "impossible"
die :: Heap Prob Int
die = asum [ writer (i, Prob (1 / 6)) | i <- [1..6] ]
\end{code}
