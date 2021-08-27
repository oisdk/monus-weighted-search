%include polycode.fmt
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"

\begin{code}
module Control.Monad.Display where

import Prelude hiding (Applicative(..),Monad(..))
\end{code}
%<*monad>
\begin{code}
class Monad m where
  pure   :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
\end{code}
%</monad>
%<*monad-plus>
\begin{code}
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
\end{code}
%</monad-plus>
%<*guard>
\begin{code}
guard :: MonadPlus m => Bool -> m ()
guard p = if p then pure () else mzero
\end{code}
%</guard>
