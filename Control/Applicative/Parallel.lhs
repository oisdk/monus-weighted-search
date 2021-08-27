%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst string a = "\AgdaString{\text{\ttfamily \char34" a "\char34}}"
%subst numeral a = "\AgdaNumber{" a "}"
%format <*> = "\hap "
%format <$> = "\hfmap "
%format _wrap = wrap

%format :& = "\mathbin{\text{\&}} "
%format bft' = bft

\begin{code}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Control.Applicative.Parallel where

import Control.Applicative
import Control.Applicative.Exp
import Control.Monad.State (evalState, get, modify)
import Data.Tree.Semiring hiding (bft)
import Prelude hiding (abs)

data Ap f a where
  Pure  :: a -> Ap f a
  Lift  :: (a -> b -> c) -> f a -> Ap f b -> Ap f c

instance Functor (Ap f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Lift g x xs) = Lift (\a b -> f (g a b)) x xs

instance Applicative f => Applicative (Ap f) where
  pure = Pure

\end{code}
%<*applicative-inst>
\begin{code}
  Pure x       <*> Pure y         = Pure (x y)
  Pure x       <*> Lift g y ys    = Lift (\y ys -> x (g y ys)) y ys
  Lift f x xs  <*> Pure y         = Lift (\x xs -> f x xs y) x xs
  Lift f x xs  <*> Lift g y ys  =
    Lift (\(x,y) (xs,ys) -> f x xs (g y ys) ) (liftA2 (,) x y) (liftA2 (,) xs ys)
\end{code}
%</applicative-inst>
\begin{code}

  liftA2 (*) (Pure x)         ys               = fmap (x *) ys
  liftA2 (*) xs               (Pure y)         = fmap (* y) xs
  liftA2 (*) (Lift (+) x xs)  (Lift (^) y ys)  =
    Lift (\(x,y) (xs,ys) -> (x + xs) * (y ^ ys)) (liftA2 (,) x y) (liftA2 (,) xs ys)
\end{code}
%<*lower>
\begin{code}
lower :: Applicative f => Ap f a -> f a
lower (Pure x)       = pure x
lower (Lift f x xs)  = liftA2 f x (lower xs)
\end{code}
%</lower>
%<*lift>
\begin{code}
lift :: f a -> Ap f a
lift x = Lift const x (Pure ())
\end{code}
%</lift>
%<*bft>
\begin{code}
bft :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bft f = lower . g where  g Tip        = pure Tip
                         g (x :& xs)  = liftA2 (:&) (lift (f x)) (wrap (traverse g xs))
\end{code}
%</bft>
%<*label>
\begin{code}
renumber :: Tree a -> Tree Int
renumber = flip evalState 0 . bft num
  where num _ = do modify (+1) ; get
\end{code}
%</label>
%<*queue>
\begin{code}
type Queue f = Cayley (Ap f)
\end{code}
%</queue>
%<*now>
\begin{code}
now :: Applicative f => f a -> Queue f a
now = abs . lift
\end{code}
%</now>
%<*later>
\begin{code}
_wrap :: Applicative f => Queue f a -> Queue f a
_wrap xs = Cayley (f xs) where  f :: Applicative f => Queue f a -> Ap f b -> Ap f (a, b)
                                f xs (Pure y)       = Lift (const id) (pure ()) (runC xs (Pure y))
                                f xs (Lift g y ys)  = Lift (fmap . g) y (runC xs ys)
\end{code}
%</later>
%<*runQueue>
\begin{code}
runQueue :: Applicative f => Queue f a -> f a
runQueue = lower . rep
\end{code}
%</runQueue>
%<*run-now-ident>
\begin{spec}
runQueue (now x)
  ==
fmap fst (lower (flip runC (Pure ()) (now x)))
  ==
fmap fst (lower (flip runC (Pure ()) (Cayley (liftA2 (,) (lift x)))))
  ==
fmap fst (lower (flip runC (Pure ()) (Cayley (liftA2 (,) (Lift const x (Pure ()))))))
  ==
fmap fst (lower (runC (Cayley (liftA2 (,) (Lift const x (Pure ())))) (Pure ())))
  ==
fmap fst (lower (liftA2 (,) (Lift const x (Pure ())) (Pure ())))
  ==
fmap fst (lower (fmap (, ()) (Lift const x (Pure ()))))
  ==
fmap fst (lower (Lift (\a b -> (,()) (const a b)) x (Pure ())))
  ==
fmap fst (lower (Lift (\a b -> (a,())) x (Pure ())))
  ==
fmap fst (liftA2 (\a b -> (a,())) x (lower (Pure ())))
  ==
fmap fst (liftA2 (\a b -> (a,())) x (pure ()))
  ==
fmap fst (fmap (\a -> (a,())) x)
  ==
fmap (fst . (\a -> (a,())) x
  ==
fmap id x
  ==
x
\end{spec}
%</run-now-ident>

%<*cayley-bft>
\begin{code}
bft' :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
bft' f = lower . rep . h where  h Tip        = pure Tip
                                h (x :& xs)  = liftA2 (:&) (abs (lift (f x))) (_wrap (traverse h xs))
\end{code}
%</cayley-bft>
%<*wrap>
\begin{code}
wrap :: Applicative f => Ap f a -> Ap f a
wrap = Lift (\ _ x -> x) (pure ())
\end{code}
%</wrap>
%<*queued>
\begin{code}
stages :: Ap IO [String]
stages = sequenceA
  [ wrap (wrap (wrap (out "a")))
  , wrap (out "b")
  , wrap (wrap (out "c"))
  , out "d"
  , wrap (out "e")]
\end{code}
%</queued>
\begin{code}

out :: String -> Ap IO String
\end{code}
%<*out>
\begin{code}
out s = lift (do putStrLn ("out: " ++ s); pure s)
\end{code}
%</out>

