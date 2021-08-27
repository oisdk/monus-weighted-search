%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"

%format HypM' m a b = a "\hypmarrow{" m "}" b
%format Bag a = "\lbagcon " a "\rbagcon"
%format Empty = "\lbagcon\rbagcon"
%format <> = "\cup"
%format >>- = "\fbind"
%format <|> = "\halt"
%format period_ = ".\;"
\begin{code}

module Control.Monad.LevelsT.Fold (LevelsT(..), liftLevelsT, concrete, collect, fromListWith, (++), (>>-), pyth) where

import Data.Bag
import Control.Applicative
import Control.Monad
import Hyper
import Prelude hiding ((++))

import qualified Control.Monad.LevelsT.Concrete as Concrete
\end{code}
%<*levelst-def>
\begin{code}
newtype LevelsT m a = LevelsT { runLevelsT :: forall r. (Bag a -> m r -> m r) -> m r -> m r }
\end{code}
%</levelst-def>
\begin{code}

instance Functor (LevelsT m) where
  fmap f xs = LevelsT (\c -> runLevelsT xs (c . fmap f))

instance Monad m => Applicative (LevelsT m) where
  pure x = LevelsT (\c n -> c (Sing x) n)
  (<*>) = ap

instance Monad m => Alternative (LevelsT m) where
  empty = LevelsT (\_ n -> n)
\end{code}
%<*alt-head>
\begin{code}
  (<|>) :: LevelsT m a -> LevelsT m a -> LevelsT m a
  xs <|> ys = LevelsT (\c n -> xz xs >>= (yz c n ys >>=))
\end{code}
%</alt-head>
\begin{code}
    where
\end{code}
%<*alt-xz>
\begin{code}
      xz xs = runLevelsT xs f b
        where
          f x xk = pure (\yk -> yk (HypM xk) x)




          b = pure (\yk -> yk (HypM b) Empty)
\end{code}
%</alt-xz>
%<*alt-yz>
\begin{code}
      yz c n ys = runLevelsT ys f (pure b)
        where
          f y yk =
            pure (\xk x ->
                    c  (x <> y)
                       (invokeM xk >>= (yk >>=)))

          b xk Empty  = n
          b xk x      = c x (invokeM xk >>= ($ b))
\end{code}
%</alt-yz>
\begin{code}
liftLevelsT :: Monad m => m (LevelsT m a) -> LevelsT m a
liftLevelsT xs = LevelsT (\c n -> xs >>= \xs' -> runLevelsT xs' c n)

wrapLevelsT :: Monad m => m (LevelsT m a) -> LevelsT m a
wrapLevelsT xs = LevelsT (\c n -> c Empty (xs >>= \xs' -> runLevelsT xs' c n))

instance Monad m => Monad (LevelsT m) where
  xs >>= k = liftLevelsT (runLevelsT xs f (pure empty))
    where
      f x xs = pure (foldr ((<|>) . k) (wrapLevelsT xs) x)

concrete :: Applicative m => LevelsT m a -> Concrete.LevelsT m a
concrete xs = Concrete.LevelsT (runLevelsT xs (\x xs -> pure (Just (x, Concrete.LevelsT xs))) (pure Nothing))

collect :: Monad m => Int -> LevelsT m a -> m [a]
collect n = Concrete.collect n . concrete

fromListWith :: Monad m => (a -> m b) -> [a] -> LevelsT m b
fromListWith f xs = LevelsT (\c n -> foldr (\y ys -> f y >>= flip (c . Sing) ys) n xs)

pyth :: IO [(Int,Int,Int)]
pyth = collect 10 $ do
  let nats = LevelsT (\c n -> foldr (\x xs -> print x >> c (Sing x) xs) n [1..])
  x <- nats
  y <- nats
  z <- nats
  guard (x * x + y * y == z * z)
  pure (x,y,z)

\end{code}
%<*plus>
\begin{code}
(++) :: LevelsT m a -> LevelsT m a -> LevelsT m a
xs ++ ys = LevelsT (\c -> runLevelsT xs c . runLevelsT ys c)
\end{code}
%</plus>
%<*fbind>
\begin{code}
(>>-) :: LevelsT m a -> (a -> LevelsT m b) -> LevelsT m b
xs >>- k = LevelsT (\c -> runLevelsT xs (flip (foldr (flip runLevelsT c . k))))
\end{code}
%</fbind>
