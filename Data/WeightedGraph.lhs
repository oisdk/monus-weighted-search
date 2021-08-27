%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"
%format !-! = "\hmonus "
%format Natural = "\hnat "
%format mempty = "\hmempty "
%format <> = "\hcmb "
%format <|> = "\halt"

%format A = a
%format B = b
%format C = c
%format D = d

%format _graph = graph
%format _graph' = graph

\begin{code}
{-# LANGUAGE TupleSections #-}

module Data.WeightedGraph
  ( dfs
  , Graph
  ) where

import Data.Dist
import Data.List

import qualified Data.Set as Set

import Control.Monad.Heap
import Control.Monad.Writer
import Control.Applicative

data ExampleGraphNode = A | B | C | D

_graph :: Graph ExampleGraphNode
\end{code}
%<*example-graph>
\aligncolumn{20}{@@{}>{\hspre}r<{\hspost}@@{}}
\begin{code}
_graph A  = [ (B,  3  ), (C, 1)]
_graph B  = [ (D,  2  )]
_graph C  = [ (D,  3  )]
_graph D  = []
\end{code}
%</example-graph>
\begin{code}
dfs :: Ord a => Graph a -> Graph a
dfs g r = go 0 r (const []) Set.empty
  where
    go d x xs s
      | Set.member x s = xs s
      | otherwise = (x, d) : foldr (uncurry (flip (go . (<>) d))) xs (sortOn snd (g x)) (Set.insert x s)

\end{code}
%<*graph-ty>
\begin{code}
type Graph a = a -> [(a, Dist)]
\end{code}
%</graph-ty>
\begin{code}
_graph' :: ExampleGraphNode -> Heap Dist ExampleGraphNode
\end{code}
%<*weight-ty>
\begin{code}
weight :: Monad m => w -> a -> HeapT w m a
\end{code}
%</weight-ty>
\begin{code}
weight w x = HeapT (pure (w :< pure x))
\end{code}
%<*example-h-graph>
\begin{code}
_graph' A  = weight 3 B <|> weight 1 C
_graph' B  = weight 2 D
_graph' C  = weight 3 D
_graph' D  = empty
\end{code}
%</example-h-graph>
