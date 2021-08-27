%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"

%format reverse1 = reverse
%format reverse2 = reverse
%format reverse3 = reverse
%format Nil = []
%format :- = :
%format >>- = "\fbind"
%format mempty = "\epsilon"
%format <> = "\hcmb"

\begin{code}
module Data.List.Display where

import Prelude hiding (reverse, abs,foldMap)

\end{code}
%<*list-def>
\begin{code}
data List a
  =  Nil
  |  a :- List a
\end{code}
%</list-def>
%<*reverse>
\begin{code}
reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]
\end{code}
%</reverse>
\begin{code}
reverse1, reverse2, reverse3 :: [a] -> [a]
\end{code}
%<*reverse1>
\begin{code}
reverse1 = foldl (flip (:)) []
\end{code}
%</reverse1>
%<*reverse2>
\begin{code}
reverse2 xs = go xs []
  where
    go []      ks = ks
    go (x:xs)  ks = go xs (x : ks)
\end{code}
%</reverse2>
%<*reverse3>
\begin{code}
type Diff a = [a] -> [a]

abs :: Diff a -> [a]
abs ks = ks []

rep :: [a] -> Diff a
rep xs ks = xs ++ ks

reverse3 xs = abs (go xs)
  where
    go []      = id
    go (x:xs)  = go xs . rep [x]
\end{code}
%</reverse3>
%<*interleave>
\begin{code}
interleave :: [a] -> [a] -> [a]
interleave []      ys = ys
interleave (x:xs)  ys = x : interleave ys xs
\end{code}
%</interleave>
%<*fair-bind>
\begin{code}
(>>-) :: [a] -> (a -> [b]) -> [b]
xs >>- k = foldr (interleave . k) [] xs
\end{code}
%</fair-bind>
\begin{code}
pyth :: [(Int,Int,Int)]
pyth = take 100 $
  [1..100] >>- \x ->
  [1..100] >>- \y ->
  [1..100] >>- \z ->
  [(x,y,z)]
\end{code}
%<*foldMap>
\begin{code}
foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f []        = mempty
foldMap f (x : xs)  = f x <> foldMap f xs
\end{code}
%</foldMap>
