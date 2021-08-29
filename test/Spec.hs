{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck hiding (tabulate)
import Test.Tasty.QuickCheck hiding (tabulate)
import Test.Tasty

import Control.Monad.Writer
import Data.List (sort)
import Data.Bifoldable
import Data.Foldable

import Control.Monad.Heap
import Control.Monad.Heap.List

import MonusWeightedSearch.Internal.AdjList

import qualified MonusWeightedSearch.Internal.Heap as H
import qualified MonusWeightedSearch.Examples.Dijkstra as M

import Data.Monus
import Data.Monus.Prob
import Data.Monus.Dist

prop_monadDijkstra :: AdjList -> Property
prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_probOrdMonoid :: Prob -> Prob -> Property
prop_probOrdMonoid x y = (x <= x <> y) .&&. (y <= x <> y)

prop_probMonus :: Prob -> Prob -> Property
prop_probMonus x y
  | x <= y    = x <> (x |-| y) === y
  | otherwise = y <> (y |-| x) === x

prop_bifoldlListCons :: Property
prop_bifoldlListCons =
  bifoldr (:) (:) [] (True :- False) === reverse (bifoldl (flip (:)) (flip (:)) [] (True :- False))

prop_fromList :: [(Word,Dist)] -> Property
prop_fromList xs = (fromList xs :: Heap Dist Word) === asum (map writer xs)


return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
