%include polycode.fmt


%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
\begin{code}
module Control.Monad.Writer.Display where

\end{code}
%<*writer-class>
\begin{code}
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    tell    :: w -> m ()
    listen  :: m a -> m (a, w)
    pass    :: m (a, w -> w) -> m a
\end{code}
%</writer-class>
