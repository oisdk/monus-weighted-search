module MonusWeightedSearch.SubsetSum where

import Control.Monad.Heap
import Data.Monus.Dist
import Control.Monad.Writer
import Control.Applicative

inclusion :: Monad m => HeapT Dist m Bool
inclusion   =    (tell 0 >> pure False)
            <|>  (tell 1 >> pure True)

shortest ::  Int -> [Int] -> [Int]
shortest t xs = head . map fst . search $ do
  subset <- filterM (const inclusion) xs
  guard (sum subset == t)
  pure subset
