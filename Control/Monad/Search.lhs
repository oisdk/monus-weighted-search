%include polycode.fmt
%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"
%subst numeral a = "\AgdaNumber{" a "}"

%format Set.notMember (x) (y) = x "\notin " y
%format Set.insert (x) (y) = y "\cup \left\{" x "\right\}"
%format Set.empty = "\left\{\right\}"
%format <|> = "\halt "
%format :< = "\mathbin{\triangleleft} "
\begin{code}
module Control.Monad.Search where

import Control.Monad.Heap
import Data.Dist
import Control.Monad.State
import Control.Applicative
import Data.Foldable
import Control.Monad.Writer
\end{code}
%<*inclusion>
\begin{code}
inclusion :: Monad m => HeapT Dist m Bool
inclusion   =    (tell 0 >> pure False)
            <|>  (tell 1 >> pure True)
\end{code}
%</inclusion>
%<*shortestSubsetSum>
\begin{code}
shortest ::  Int -> [Int] -> [Int]
shortest t xs = head . map fst . search $ do
  subset <- filterM (const inclusion) xs
  guard (sum subset == t)
  pure subset
\end{code}
%</shortestSubsetSum>
\begin{code}
dsort :: [Dist] -> [Dist]
dsort = map snd . search . asum . map tell
{-# INLINE dsort #-}
\end{code}
