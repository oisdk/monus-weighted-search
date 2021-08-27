%include polycode.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format pattern = "\textcolor{AHKeywordColour}{\textbf{pattern}}"

%format +. = +
%format *. = *

\begin{code}
module Data.Poly.Display where

import Numeric.Natural

type Poly = [Natural]


(+.), (*.) :: Poly -> Poly -> Poly
\end{code}
%<*add>
\begin{code}
[]      +. ys      = ys
xs      +. []      = xs
(x:xs)  +. (y:ys)  = (x + y) : (xs +. ys)
\end{code}
%</add>
%<*mult>
\begin{code}
[]      *. ys  = []
(x:xs)  *. ys  = map (x *) ys +. (0 : (xs *. ys))
\end{code}
%</mult>
