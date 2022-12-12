-- |
-- Module      : MonusWeightedSearch.Examples.Parsing
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A probabilistic parser, implemented using the 'Heap' monad.
--
-- The main interesting thing about this parser is that it is defined by
-- flipping the order of 'State' and 'Heap' in the monad stack (the other way
-- around you get a monad for things like Dijkstra's algorithm,
-- "MonusWeightedSearch.Examples.Dijkstra").
--
-- The parser itself is a /probabilistic/ parser, meaning that it can have a
-- preference for certain parse trees over others, based on their likelihood.
-- When the parser is run the output is listed in order of each likelihood.

module MonusWeightedSearch.Examples.Parsing where

import Control.Applicative
import Control.Monad.Heap
import Control.Monad.State
import Data.Monus.Prob
import Control.Monad.Writer
import Control.Monad (guard)

-- | A standard parser type.
--
-- Compare to @type Parser a b = [a] -> [(b, [a])]@: we have swapped out the
-- list here for the heap, allowing for efficient ordering of results.
type Parser a = StateT [a] (Heap Prob)

-- | Parse an empty string.
eof :: Parser a ()
eof = StateT \case
  [] -> pure ((), [])
  _ -> empty

-- | Parse a single char.
anyChar :: Parser a a
anyChar = StateT \case
  (x:xs) -> pure (x, xs)
  [] -> empty

-- | Filter the output of a parse.
satisfy :: (b -> Bool) -> Parser a b -> Parser a b
satisfy p xs = do
  x <- xs
  guard (p x)
  pure x

-- | Assign a parse result a /probability/: when the parser is run, it will
-- order results from most to least likely.
condition :: (b -> Prob) -> Parser a b -> Parser a b
condition c xs = do
  x <- xs
  tell (c x)
  pure x

-- | Parse a string, ordering the results from most to least likely.
parse :: Parser a b -> [a] -> [(b, Prob)]
parse p xs = search (evalStateT (p <* eof) xs)
