%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"

%format <|> = "\halt"
%format :& = "\mathbin{\text{\&}} "
%format _exampleTree = tree
%format _label = label
%format :< = "\mathbin{\triangleleft} "
%format <*> = "\hap "
%format :& = "\mathbin{\&} "
%format >< = "\bowtie "
%format ~ = "\;{\sim}"
%format period_ = ".\;"
%format +++ = "\plussdot"

\begin{code}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Data.Tree.Semiring
  (Tree(..),root,children,bft,dft,levels,labelWith,choices,dfe,poe) where

import Test.QuickCheck
import TestHelpers
import Control.Applicative
import Control.Monad.State
\end{code}
%<*tree-def>
\begin{code}
data Tree a = Tip | a :& [Tree a]
\end{code}
%</tree-def>
\begin{code}
  deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (Tree a)
deriving instance Ord a => Ord (Tree a)
deriving instance Show a => Show (Tree a)

root :: Tree a -> Maybe a
root (x :& xs) = Just x
root Tip      = Nothing

children :: Tree a -> [Tree a]
children (_ :& xs) = xs
children Tip      = []

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go  n = frequency [(1, pure Tip), (n, liftA2 (:&) arbitrary (go' (n-1)))]
      go' n = sumsTo n >>= traverse go

_exampleTree, _dfTree :: Tree Int
\end{code}
\begin{code}
\end{code}
%<*example-tree>
\begin{code}
_exampleTree = 1 :&  [  2  :&  [  5  :&  [  9   :& []
                                         ,  10  :& []]
                               ,  6  :&  []]
                     ,  3  :&  []
                     ,  4  :&  [  7  :&  [  11  :& []
                                         ,  12  :& []]
                               ,  8  :&  []]]
\end{code}
%</example-tree>
\begin{code}
_dfTreeChar :: Tree Char
_dfTreeChar =
\end{code}
%<*example-tree-char>
\begin{code}
  'a' :&  [  'b' :&  [  'c'  :&  [  'd' :& []
                                 ,  'e' :& []]
                     ,  'f'  :&  []]
          ,  'g' :&  []
          ,  'h' :&  [  'i'  :&  [  'j' :& []
                                 ,  'k' :& []]
                     ,  'l'  :&  []]]
\end{code}
%</example-tree-char>
%<*labelWith>
%<*dfe>
\begin{code}
dfe :: Tree a -> [a]
dfe Tip        = []
dfe (x :& xs)  = [x] ++ choices dfe xs
\end{code}
%</dfe>
%<*dfe-spec>
\begin{spec}
dfe _exampleTree == [1,2,5,9,10,6,3,4,7,11,12,8]
\end{spec}
%</dfe-spec>
%<*poe>
\begin{code}
poe :: Tree a -> [a]
poe Tip        = []
poe (x :& xs)  = [x] +++ choices poe xs
  where lhs +++ rhs = rhs ++ lhs
\end{code}
%</poe>
%<*poe-spec>
\begin{spec}
poe _exampleTree == [9,10,5,6,2,3,11,12,7,8,4,1]
\end{spec}
%</poe-spec>
%<*choices>
\begin{code}
choices :: Alternative f => (a -> f b) -> [a] -> f b
choices f []      = empty
choices f (x:xs)  = f x <|> choices f xs
\end{code}
%</choices>
\begin{code}
_dfTree =
\end{code}
%<*df-tree>
\begin{code}
  1 :&  [  2 :&  [  3  :&  [  4 :& []
                           ,  5 :& []]
                 ,  6  :&  []]
        ,  7 :&  []
        ,  8 :&  [  9  :&  [  10 :& []
                           ,  11 :& []]
                 ,  12 :&  []]]
\end{code}
%</df-tree>
\begin{code}
labelWith ::
  (forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)) ->
  Tree a -> Tree Int
labelWith trav =  flip evalState 0 .
                  trav (\ _ -> do modify succ ; get)
\end{code}
%</labelWith>
%<*dft>
\begin{code}
dft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
dft f Tip       = pure Tip
dft f (x :& xs)  = liftA2 (:&) (f x) (traverse (dft f) xs)
\end{code}
%</dft>
%<*relabel-df>
\begin{code}
_label :: Tree a -> Tree Int
_label =  flip evalState 0 .
          traverse (\ _ -> do modify succ ; get)
\end{code}
%</relabel-df>
%<*bft>
\begin{code}
bft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bft f Tip        = pure Tip
bft f (x :& xs)  = liftA2 (:&) (f x) (bftF f xs)

bftF :: Applicative f => (a -> f b) -> [Tree a] -> f [Tree b]
bftF t = fmap head . foldr (<*>) (pure []) . foldr f [pure ([]:)] where
    f Tip       qs         = qs
    f (x :& xs)  ~(q : qs)  = liftA2 (><) (t x) q : foldr f (p qs) xs

    p []      = [pure ([]:)]
    p (x:xs)  = fmap (([]:).) x : xs

    (x >< k) ~(xs : ks) = ((x :& xs) : y) : ys where ~(y : ys) = k ks
\end{code}
%</bft>
\begin{code}
levels :: Tree a -> [[a]]
levels t = takeWhile (not . null) (f t (repeat []))
  where
    f Tip       qs         = qs
    f (x :& xs)  ~(q : qs)  = (x : q) : foldr f qs xs
\end{code}


