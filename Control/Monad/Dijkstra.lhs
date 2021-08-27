%include polycode.fmt
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"

%format Set.notMember (x) (y) = x "\notin " y
%format Set.insert (x) (y) = "\left\{" x "\right\} \cup"
%format Set.empty = "\left\{\right\}"
%format <|> = "\halt "
%format <=< = "\hkcomp"
%format :< = "\mathbin{\triangleleft} "
\begin{code}
module Control.Monad.Dijkstra (unique, dijkstra) where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Star
import Control.Monad.Writer

import Data.Dist
import Data.Set (Set)
import qualified Data.Set as Set

import Data.WeightedGraph

import Control.Monad.Heap
\end{code}
%<*unique>
\begin{code}
unique :: Ord a => a -> HeapT w (State (Set a)) a
unique x = do  seen <- get
               guard (Set.notMember x seen)
               modify (Set.insert x)
               pure x
\end{code}
%</unique>
\begin{code}
{-# INLINE unique #-}
\end{code}
%<*dijkstra>
\begin{code}
dijkstra :: Ord a => (a -> [(a, Dist)]) -> a -> [(a, Dist)]
dijkstra g x =
    evalState (searchT (star (ugraph g) =<< unique x)) Set.empty
\end{code}
%</dijkstra>
\begin{code}
  where ugraph g = choices (\(x,w) -> tell w >> unique x) . g
{-# INLINE dijkstra #-}

choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f = foldr ((<|>) . f) empty
{-# INLINE choices #-}
\end{code}
