%include polycode.fmt

%format <|> = "\halt "
\begin{code}
module Control.Monad.Star where

import Control.Applicative
import Control.Monad

\end{code}
%<*star-def>
\begin{code}
star :: MonadPlus m => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)
\end{code}
%</star-def>
\begin{code}

plus :: MonadPlus m => (a -> m a) -> a -> m a
plus f x = f x >>= star f
\end{code}
