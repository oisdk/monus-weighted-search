{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck hiding (tabulate)
import Test.Tasty.QuickCheck hiding (tabulate)
import Test.Tasty

import Data.List (sort)
import Data.Foldable (toList)
import Control.Monad (ap,join)
import Control.Applicative ((<|>),liftA2)

import Data.WeightedGraph.AdjList

-- import qualified Data.Heap as H
-- import qualified Control.Monad.Dijkstra as M
-- import qualified Control.Monad.Heap as MH


import Data.Monus
import Data.Monus.Prob
-- import Data.Monus.Dist


-- prop_monadDijkstra :: AdjList -> Property
-- prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_probOrdMonoid :: Prob -> Prob -> Property
prop_probOrdMonoid x y = (x <= x <> y) .&&. (y <= x <> y)

prop_probMonus :: Prob -> Prob -> Property
prop_probMonus x y
  | x <= y    = x <> (x |-| y) === y
  | otherwise = y <> (y |-| x) === x

return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
