%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%format -&> = "\hyparrow "
%format HypM' m a b = a "\hypmarrow{" m "}" b
%format _zip = zip
%format _zip' = zip
%format _zip'' = zip
%format _zip''' = zip
%format hypBfe = bfe
%format :& = "\mathbin{\&}"

\begin{code}

module Hyper (type (-&>)(..), HypM'(..), hypBfe) where

import Data.Tree.Semiring (Tree(..))

infixr 1 -&>
\end{code}
%<*hyper-def>
\begin{code}
newtype a -&> b = Hyp { invoke :: (b -&> a) -> b }
\end{code}
%</hyper-def>
%<*hypert-def>
\begin{code}
newtype HypM' m a b = HypM { invokeM :: m ((HypM' m a b -> a) -> b) }
\end{code}
%</hypert-def>
%<*zip>
\begin{code}
_zip :: [a] -> [b] -> [(a,b)]
_zip xs ys = invoke (xz xs) (yz ys)
\end{code}
%</zip>
%<*xz>
\begin{code}
xz :: [a] -> (a -> [(a,b)]) -&> [(a,b)]
xz = foldr f b where
  f x xk = Hyp (\yk -> invoke yk xk x)
  b = Hyp (\ _ -> [])
\end{code}
%</xz>
%<*yz>
\begin{code}
yz :: [b] -> [(a,b)] -&> (a -> [(a,b)])
yz = foldr f b where
  f y yk = Hyp (\xk x -> (x,y) : invoke xk yk)
  b = Hyp (\ _ _ -> [])
\end{code}
%</yz>
%<*bfe>
\begin{code}
hypBfe :: Tree a -> [a]
hypBfe t = extract (br t (HypM Nothing))
  where
    br Tip        q = q
    br (x :& xs)  q = HypM (Just (\qs -> x : unfold q (qs . flip (foldr br) xs)))

    extract (HypM Nothing)   = []
    extract (HypM (Just k))  = k extract

    unfold (HypM Nothing)   = \k -> k (HypM Nothing)
    unfold (HypM (Just f))  = f
\end{code}
%</bfe>
%<*normal-zip>
\begin{code}
_zip' :: [a] -> [b] -> [(a,b)]
_zip' []      ys      = []
_zip' xs      []      = []
_zip' (x:xs)  (y:ys)  = (x,y) : _zip' xs ys
\end{code}
%</normal-zip>
%<*wzip>
\begin{code}
_zip'' :: [a] -> [b] -> [(a,b)]
_zip'' xs ys = xz xs ys
\end{code}
%</wzip>
\begin{code}
  where
\end{code}
%<*wxz>
\begin{code}
    xz [] _       = []
    xz (x:xs) ys  = yz ys xs x
\end{code}
%</wxz>
%<*wyz>
\begin{code}
    yz [] _ _       = []
    yz (y:ys) xs x  = (x,y) : xz xs ys
\end{code}
%</wyz>

%<*ezip>
\begin{spec}
zip :: [a] -> [b] -> [(a,b)]
zip xs ys = (xz xs) (yz ys)
\end{spec}
%</ezip>
%<*exz>
\begin{spec}
xz = foldr f b where
  b _ = []
  f x xk yk = yk xk x
\end{spec}
%</exz>
%<*eyz>
\begin{spec}
yz = foldr f b where
  b _ _ = []
  f y yk xk x = (x,y) : xk yk
\end{spec}
%</eyz>
%<*zip-ty>
\begin{spec}
newtype Zip a b = Zip { runZip :: a -> (Zip a b -> [(a,b)]) -> [(a,b)] }
\end{spec}
%</zip-ty>
