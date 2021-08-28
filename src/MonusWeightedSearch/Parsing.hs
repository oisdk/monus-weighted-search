module MonusWeightedSearch.Parsing where

import Control.Applicative
import Control.Monad.Heap
import Control.Monad.State
import Data.Monus.Prob
import Control.Monad.Writer

type Parser a = StateT [a] (Heap Prob)

eof :: Parser a ()
eof = StateT \case
  [] -> pure ((), [])
  _ -> empty

anyChar :: Parser a a
anyChar = StateT \case
  (x:xs) -> pure (x, xs)
  [] -> empty

satisfy :: (b -> Bool) -> Parser a b -> Parser a b
satisfy p xs = do
  x <- xs
  guard (p x)
  pure x

condition :: (b -> Prob) -> Parser a b -> Parser a b
condition c xs = do
  x <- xs
  tell (c x)
  pure x
  
parses :: Parser a b -> [a] -> [(b, Prob)]
parses p xs = search (evalStateT (p <* eof) xs)
