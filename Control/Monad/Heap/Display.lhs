%include polycode.fmt

%format <> = "\hcmb "
%format mempty = "\hmempty "
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format >+< = "\hmerge"
%format >-< = "\hmerge"
%format W' = W
%format :< = "\mathbin{\triangleleft} "
%format =~= = "\simeq"

%format _wGraph = wGraph

%format x1
%format x2
%format x3

%format x_w
%format y_w

%format (diff (x) (y)) = "\lvert" x "-" y "\rvert"

\begin{code}
module Control.Monad.Heap.Display where

import Data.Bifunctor
import Data.Node
import Data.Monus
import Data.Dist
import Data.WeightedGraph
import Control.Applicative
import Control.Monad

choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f = foldr ((<|>) . f) empty

instance Functor (Heap w) where
  fmap f = Heap . map (bimap f (fmap f)) . runHeap
instance Applicative (Heap w) where
  pure x = Heap [Leaf x]
  (<*>) = ap
instance Monad (Heap w) where
  xs >>= f = Heap (runHeap xs >>= g)
    where
      g (Leaf x) = runHeap (f x)
      g (w :< xs) = [w :< (xs >>= f)]
instance Alternative (Heap w) where
  empty = Heap []
  Heap xs <|> Heap ys = Heap (xs ++ ys)
\end{code}
%<*fromGraph>
\begin{code}
fromGraph :: Graph a -> a -> Heap Dist a
fromGraph g = choices (\(x,w) -> Heap [w :< Heap [Leaf x]]) . g
\end{code}
%</fromGraph>
%<*flat-w-ty>
\begin{code}
newtype Heap w a =
  Heap { runHeap :: [Node w a (Heap w a)] }
\end{code}
%</flat-w-ty>
%<*wrong-w-ty>
\begin{spec}
data Heap w a = w :< [Either a (Heap w a)]
\end{spec}
%</wrong-w-ty>
%<*freet-def>
\begin{code}
newtype FreeT f m a = FreeT { runFreeT :: m (Either a (f (FreeT f m a))) }
\end{code}
%</freet-def>
%<*free-plus-def>
\begin{code}
type FreePlus f = FreeT f []
\end{code}
%</free-plus-def>
%<*weighted-def>
\begin{spec}
Heap w a =~= FreePlus ((,) w) a =~= FreeT ((,) w) [] a
\end{spec}
%</weighted-def>
%<*popMin-ty>
\begin{code}
popMin :: Ord w => Heap w a -> ([a], Maybe (w, Heap w a))
\end{code}
%</popMin-ty>
%<*popMin-impl>
\begin{code}
popMin = second comb . partition . runHeap
\end{code}
%</popMin-impl>
%<*partition>
\begin{code}
partition :: [Node w a b] -> ([a], [(w, b)])
partition = foldr f ([],[]) where
    f (Leaf x)  (xs,ys)  = (x:xs,ys)
    f (w :< y)  (xs,ys)  = (xs,(w,y):ys)
\end{code}
%</partition>
\begin{code}
\end{code}
%<*comb-ty>
\begin{code}
comb :: Ord w => [(w, Heap w a)] -> Maybe (w, Heap w a)
\end{code}
%</comb-ty>
%<*comb-impl-head>
\begin{code}
comb []      = Nothing
comb (x:xs)  = Just (go x xs)
\end{code}
%</comb-impl-head>
\begin{code}
  where
\end{code}
%<*comb-impl-go>
\begin{code}
    go x   []              = x
    go x1  [x2]            = x1 >+< x2
    go x1  (x2 : x3 : xs)  = (x1 >+< x2) >+< go x3 xs
\end{code}
%</comb-impl-go>
%<*merge>
\begin{code}
(>+<) :: Ord w => (w, Heap w a) -> (w, Heap w a) -> (w, Heap w a)
(x_w, Heap xs) >+< (y_w, Heap ys)
  | x_w <= y_w  = (x_w, Heap ((y_w :< Heap ys) : xs))
  | otherwise   = (y_w, Heap ((x_w :< Heap xs) : ys))
\end{code}
%</merge>
%<*weighted-heap>
\begin{spec}
Maybe (w, Heap w a)
\end{spec}
%</weighted-heap>
\begin{code}
(>-<) :: Monus w => (w, Heap w a) -> (w, Heap w a) -> (w, Heap w a)
\end{code}
%<*merge-subtract>
\begin{code}
(x_w, Heap xs) >-< (y_w, Heap ys)
  | x_w <= y_w  = (x_w, Heap ((diff x_w y_w :< Heap ys) : xs))
  | otherwise   = (y_w, Heap ((diff x_w y_w :< Heap xs) : ys))
\end{code}
%</merge-subtract>
