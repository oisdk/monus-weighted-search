%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"
%format pattern = "\textcolor{AHKeywordColour}{\textbf{pattern}}"

%format Levels' = Levels
%format Bag (a) = "\lbagcon " a "\rbagcon"
%format :~ = "\colonsim "
%format Empty = "\lbagcon\rbagcon"
%format Sing (a) = "\lbagcon " a "\rbagcon "
%format rsing (a) = "\lbagcon " a "\!\rbagcon "
%format <> = "\mathbin{\cup}\,"
%format <|> = "\halt "
%format <*> = "\hap "
%format <$> = "\hfmap "
%format =~= = "\mathbin{\equiv_{\scriptscriptstyle{/ \ldots 0}}}"
%format _badBind (x) (y) = x "\bind" y
%format _add x y = x + y
%format _mult x y = x * y
%format _levels1 = bfe
%format _levels2 = bfe
%format :& = "\mathbin{\text{\&}} "
%format _bfe = bfe
%format <++> = "\plussdotdot"
%format Natural = "\Nat"
%format _nats = nats
%format x0
%format x1
%format x2
%format x_n
%format _mult (x) (y) = x "\times" y
%format _pyth = pyth
%format * = "\times"
%format pow (x) (n) = "\ensuremath{" x "^{" n "}}"

%format A = a
%format B = b
%format C = c
%format _diag = diag

\begin{code}
module Control.Monad.Levels (Levels, pattern Levels, runLevels, pattern (:!), pattern None, pattern (:~),pyth,wrap) where

import Data.Bag hiding (fromList)
import Test.QuickCheck
import Control.Applicative
import Control.Monad
import Data.Function (on)
import Numeric.Natural
import Data.Coerce.Operators
import Data.Tree.Semiring

\end{code}
%<*levels-def>
\begin{code}
newtype Levels a =
  Levels' [Bag a]
\end{code}
%</levels-def>
\begin{code}
  -- ^The 'Levels' type represents nondeterministic computations
  --  where each level is explored in turn. It can be used to implement
  --  breadth-first search.
  deriving stock (Functor, Foldable, Traversable)

instance (Ord a, Show a) => Show (Levels a) where
  showsPrec p xs =
    showParen (p > 10) $
      showString "Levels " . shows (runLevels xs)

pattern Levels :: [Bag a] -> Levels a
pattern Levels {runLevels} <- (normalise -> runLevels)
  where
    Levels = Levels'
{-# COMPLETE Levels #-}

getLevels :: Levels a -> [Bag a]
getLevels (Levels' xs) = xs

normalise :: Levels a -> [Bag a]
normalise = foldr f [] . getLevels
  where
    f Empty [] = []
    f x     xs = x : xs

instance Arbitrary a => Arbitrary (Levels a) where
  arbitrary = Levels' <#$> arbitrary
  shrink = map Levels' #. (shrink .# getLevels)
\end{code}
%<*trunc-eq>
\begin{code}
(=~=) :: Ord a => [Bag a] -> [Bag a] -> Bool
[]      =~= ys      = all (Empty ==) ys
xs      =~= []      = all (Empty ==) xs
(x:xs)  =~= (y:ys)  = (x == y) && (xs =~= ys)
\end{code}
%</trunc-eq>
%<*eq-inst>
\begin{code}
instance Ord a => Eq (Levels a) where
  Levels' xs == Levels' ys = xs =~= ys
\end{code}
%</eq-inst>
\begin{code}

instance Ord a => Ord (Levels a) where
  compare = compare `on` normalise

infixr 5 :~
uncons :: Levels a -> (Bag a, Levels a)
\end{code}
%<*uncons-patt>
\begin{code}

pattern (:~) :: Bag a -> Levels a -> Levels a
pattern (:~) x xs <- (uncons -> ~(x, xs))
  where x :~ Levels' xs = Levels' (x : xs)

uncons (Levels' (x:xs))  = (x      , Levels' xs)
uncons (Levels' [])      = (Empty  , Levels' [])
\end{code}
%</uncons-patt>
\begin{code}
{-# COMPLETE (:~) #-}

unconsLevelsStrict :: Levels a -> Maybe (Bag a, Levels a)
unconsLevelsStrict (Levels' []) = Nothing
unconsLevelsStrict xs | null xs = Nothing
unconsLevelsStrict (Levels' (x:xs)) = Just (x, Levels' xs)

consLevelsStrict :: Bag a -> Levels a -> Levels a
consLevelsStrict Empty xs | null xs = Levels' []
consLevelsStrict x     (Levels' xs) = Levels' (x : xs)

pattern None :: Levels a
pattern None <- (unconsLevelsStrict -> Nothing)
  where
    None = Levels' []

infixr 5 :!
pattern (:!) :: Bag a -> Levels a -> Levels a
pattern (:!) x xs <- (unconsLevelsStrict -> Just (x, xs))
  where
    (:!) = consLevelsStrict
{-# COMPLETE (:!), None #-}

sumL :: Foldable f => (a -> Levels b) -> f a -> Levels b
\end{code}
%<*wrap>
\begin{code}
wrap :: Levels a -> Levels a
wrap (Levels' xs) = Levels' (Empty : xs)
\end{code}
%</wrap>
\begin{code}
sumL f = foldr ((<|>) . f) empty
\end{code}
\begin{code}

instance Applicative Levels where
\end{code}
%<*pure>
\begin{code}
    pure :: a -> Levels a
    pure x = Levels' [Sing x]
\end{code}
%</pure>
%<*applicative>
\begin{code}
    (<*>) :: Levels (a -> b) -> Levels a -> Levels b
    Levels' []      <*> _           = Levels' []
    Levels' (x:xs)  <*> Levels' ys  = Levels' (map (x <*>) ys) <|> wrap (Levels' xs <*> Levels' ys)
\end{code}
%</applicative>
\begin{code}
\end{code}
%<*monad>
\begin{code}
instance Monad Levels where
  Levels' []        >>= k = empty
  Levels' (xs:xss)  >>= k = choices' k xs <|> wrap (Levels' xss >>= k)
\end{code}
%</monad>
\begin{code}

choices' :: (a -> Levels b) -> Bag a -> Levels b
choices' k = foldr ((<|>) . k) empty


_badBind :: Levels a -> (a -> Levels b) -> Levels b
\end{code}
%<*delay>
\begin{code}
delay :: Int -> Levels a -> Levels a
delay 0  xs = xs
delay i  xs = wrap (delay (i-1) xs)
\end{code}
%</delay>
%<*badBind>
\begin{code}
_badBind (Levels xs) k =
  sumL  (\(i,x) -> delay i (sumL k x))
        (zip [0..] xs)
\end{code}
%</badBind>
%<*bind-opt>
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{spec}
sumL (\(i,x) -> delay i (sumL k x)) (zip [0..] xs)
  == {- apply |zip| -}
sumL (\(i,x) -> delay i (sumL k x)) [(0,x0),(1,x1),(2,x2)...
  == {- apply |sumL| -}
delay 0 (sumL k x0) <|> delay 1 (sumL k x1) <|> delay 2 (sumL k x2) <|> ...
  == {- apply |delay| -}
sumL k x0 <|> wrap (sumL k x1) <|> wrap (wrap (sumL k x2)) <|> ...
  == {- \(\Varid{wrap}\) distributes over \(\halt\) -}
sumL k x0 <|> wrap (sumL k x1 <|> wrap (sumL k x2 <|> ... ))
  == {- definition of |foldr| -}
foldr (\x ys -> sumL k x <|> wrap ys) empty xs
\end{spec}
%</bind-opt>
%<*alternative>
\begin{code}
instance Alternative Levels where
  empty = Levels' []
  Levels' xs <|> Levels' ys = Levels' (zipL xs ys)
\end{code}
%</alternative>
\begin{code}
    where
\end{code}
%<*long-zip>
\begin{code}
      zipL []      ys      = ys
      zipL xs      []      = xs
      zipL (x:xs)  (y:ys)  = (x <> y) : zipL xs ys
\end{code}
%</long-zip>
A much lazier alternative:
\begin{spec}
instance Alternative Levels where
  empty = Levels' []

  Levels' xs <|> ys = foldr f id xs ys
    where
      f x k (y :~ ys) = (x <> y) :~ k ys
\end{spec}
\begin{code}
instance MonadPlus Levels

instance Semigroup (Levels a) where
  (<>) = (<|>)

instance Monoid (Levels a) where
  mempty = empty

\end{code}
%<*nats>
\begin{code}
nats :: Levels Natural
nats = Levels [ Sing n | n <- [1..] ]
\end{code}
%</nats>
\begin{code}
_nats :: Levels Natural
\end{code}
%<*nats-alt>
\begin{code}
_nats = pure 0 <|> wrap (fmap succ _nats)
\end{code}
%</nats-alt>
%<*nats-spec>
\begin{spec}
nats == Levels [ Sing 0, Sing 1, Sing 2, Sing 3 .. ]
\end{spec}
%</nats-spec>
\begin{code}
pow :: Natural -> Natural -> Natural
pow = (^)
\end{code}
%<*pyth>
\begin{code}
pyth :: Levels (Natural, Natural, Natural)
pyth = do
  x <- nats
  y <- nats
  z <- nats
  guard (pow x 2 + pow y 2 == pow z 2)
  pure (x,y,z)
\end{code}
%</pyth>
%<*pyth-fin>
\begin{code}
_pyth :: [(Natural, Natural, Natural)]
_pyth = do
  x <- [1..10]
  y <- [1..10]
  z <- [1..10]
  guard (pow x 2 + pow y 2 == pow z 2)
  pure (x,y,z)
\end{code}
%</pyth-fin>
%<*tree-to-levels1>
\begin{code}
_levels1 :: Tree a -> Levels a
_levels1 Tip        = empty
_levels1 (x :& xs)  =
  pure x <|> wrap (sumL _levels1 xs)
\end{code}
%</tree-to-levels1>
%<*tree-to-levels2-head>
\begin{code}
_levels2 :: Tree a -> Levels a
_levels2 t = Levels (f t [])
\end{code}
%</tree-to-levels2-head>
\begin{code}
  where
\end{code}
%<*tree-to-levels2-impl>
\begin{code}
    f Tip        qs      = qs
    f (x :& xs)  (q:qs)  = rsing x <> q  : foldr f qs xs
    f (x :& xs)  []      = rsing x       : foldr f [] xs
\end{code}
%</tree-to-levels2-impl>
\begin{code}
    rsing = Sing
_badL :: Levels Int
_badL =
\end{code}
%<*bad-levels>
\begin{code}
   Levels [Sing 1]                     <|>
   Levels [Empty,Sing 2]               <|>
   Levels [Empty,Empty,Sing 5]         <|>
   Levels [Empty,Empty,Empty,Sing 9]   <|>
   Levels [Empty,Empty,Empty,Sing 10]  <|>
   Levels [Empty,Empty,Sing 6]         <|>
   Levels [Empty,Sing 3]               <|>
   Levels [Empty,Sing 4]               <|>
   Levels [Empty,Empty,Sing 7]         <|>
   Levels [Empty,Empty,Empty,Sing 11]  <|>
   Levels [Empty,Empty,Empty,Sing 12]  <|>
   Levels [Empty,Empty,Sing 8]         <|>
   Levels []
\end{code}
%</bad-levels>
%<*bfe>
\begin{code}
_bfe :: Tree a -> Levels a
_bfe Tip        = Levels' []
_bfe (x :& xs)  = pure x <++> choices _bfe xs
  where lhs <++> rhs = lhs <|> wrap rhs
\end{code}
%</bfe>
\begin{code}
_mult :: Levels () -> Levels () -> Levels ()
\end{code}
%<*mult>
\begin{code}
_mult xs ys = do xs; ys
\end{code}
%</mult>
\begin{code}
data Letter = A | B | C deriving (Eq, Ord, Show)
_diag :: Levels (Natural,Letter)
\end{code}
%<*diag>
\begin{code}
_diag = do
  x <- Levels [Sing 1, Sing 2, Sing 3]
  y <- Levels [Sing A, Sing B, Sing C]
  pure (x,y)
\end{code}
%</diag>
