%include polycode.fmt

%format <> = "\hcmb "
%format mempty = "\hmempty "
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"


\begin{code}
module Control.Monad.Free.Display where

import Control.Monad.List.Trans
\end{code}
%<*free-plus>
\begin{code}
newtype Free f a = Free { runFree :: [Either a (f (Free f a))] }
\end{code}
%</free-plus>
%<*freet-plus>
\begin{code}
newtype FreeT f m a
  = FreeT { runFreeT :: ListT m (Either a (f (FreeT f m a))) }
\end{code}
%</freet-plus>
%<*heap>
\begin{code}
newtype HeapT w m a
  = HeapT { runHeapT :: ListT m (Either a (w, HeapT w m a)) }
\end{code}
%</heap>
