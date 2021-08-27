%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"


\begin{code}
module Data.Traversable.Display where

import Prelude hiding (Traversable(..))

\end{code}
%<*traversable-def>
\begin{code}
class Foldable t => Traversable t where traverse :: Applicative f => (a -> f b) -> t a -> f (t a)
\end{code}
%</traversable-def>
