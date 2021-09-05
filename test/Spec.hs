{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Test.QuickCheck hiding (tabulate)
import Test.Tasty.QuickCheck hiding (tabulate)
import Test.Tasty

import Control.Monad.Writer
import Data.List (sort)
import Data.Bifoldable
import Data.Foldable
import Control.Applicative

import Control.Monad.Heap
import Control.Monad.Heap.List

import MonusWeightedSearch.Internal.AdjList

import qualified MonusWeightedSearch.Internal.Heap as H
import qualified MonusWeightedSearch.Examples.Dijkstra as M
import qualified MonusWeightedSearch.Examples.Sort as M

import Data.Monus
import Data.Monus.Prob
import Data.Monus.Dist

prop_monadDijkstra :: AdjList -> Property
prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_monadSort :: [Dist] -> Property
prop_monadSort xs = sort xs === M.monusSort xs

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

data Pair a = a :*: a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Arbitrary1 Pair where
  liftArbitrary arb = liftA2 (:*:) arb arb
  liftShrink shr (x :*: y) = map (uncurry (:*:)) (liftShrink2 shr shr (x , y))

prop_traverseHeap :: Property
prop_traverseHeap = mapSize (min 5) $
  forAll (arbitrary :: Gen (HeapT Dist Pair Word))
    (\xs -> foldr (:) [] xs === appEndo (fst (traverse (\x -> (Endo (x:), ())) xs)) [])

return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
