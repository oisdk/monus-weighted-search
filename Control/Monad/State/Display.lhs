%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"

\begin{code}
module Control.Monad.State.Display where

newtype State s a = State { runState :: s -> (a, s) } deriving Functor

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  fs <*> xs = State (\s -> case runState fs s of (f, s') -> case runState xs s' of (x, s'') -> (f x, s''))

instance Monad (State s) where
  xs >>= f =
    State (uncurry (runState . f) . runState xs)

\end{code}
%<*state-ops>
\begin{code}
evalState  :: State s a -> s -> a
modify     :: (s -> s) -> State s ()
get        :: State s s
\end{code}
%</state-ops>
\begin{code}
evalState xs = fst . runState xs

modify f = State (\s -> ((), f s))

get = State (\s -> (s, s))
\end{code}
