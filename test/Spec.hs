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

import qualified Data.Heap as H
import qualified Control.Monad.Dijkstra as M
import qualified Control.Monad.Heap as MH


import qualified Control.Monad.LevelsT.Fold as LM

import Control.Monad.Levels
import Data.Tree.Semiring
import Data.Prob
import Data.Dist
import Data.Monus

import qualified Hyper

tellLevels :: [a] -> LM.LevelsT ((,) [a]) a
tellLevels = LM.fromListWith (\x -> ([x], x))

prop_levelsFair :: Property
prop_levelsFair =
  (LM.collect 6 (tellLevels [(11 :: Int)..] <|> tellLevels [21..] <|> tellLevels [31..]) ===
  ([11,21,31,12,22,32],[11,21,31,12,22,32])) .&&.
  (LM.collect 5 (tellLevels [(11 :: Int)..] <|> tellLevels [21..] <|> tellLevels [31..]) ===
  ([11,21,31,12,22,32],[11,21,31,12,22]))

prop_levelsAssoc :: Property
prop_levelsAssoc =
  LM.collect 5 ((tellLevels [(11 :: Int)..] <|> tellLevels [21..]) <|> tellLevels [31..]) ===
  LM.collect 5 (tellLevels [(11 :: Int)..] <|> (tellLevels [21..] <|> tellLevels [31..]))

prop_hyperBfe :: Tree Word -> Property
prop_hyperBfe t = concat (levels t) === Hyper.hypBfe t

prop_monadDijkstra :: AdjList -> Property
prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_probOrdMonoid :: Prob -> Prob -> Property
prop_probOrdMonoid x y = (x <= x <> y) .&&. (y <= x <> y)

prop_probMonus :: Prob -> Prob -> Property
prop_probMonus x y
  | x <= y    = x <> (x |-| y) === y
  | otherwise = y <> (y |-| x) === x

prop_levelsPyth :: Property
prop_levelsPyth = take 5 (toList pyth) === [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]

prop_levelsBindRightIdCase :: Property
prop_levelsBindRightIdCase = prop_levelsBindRightId (Levels [mempty])

prop_levelsBindLeftIdCase :: Property
prop_levelsBindLeftIdCase = prop_levelsBindLeftId (Levels [])

prop_levelsBindRightId :: Levels Bool -> Property
prop_levelsBindRightId xs = xs === join (fmap pure xs)

prop_levelsBindLeftId :: Levels Bool -> Property
prop_levelsBindLeftId xs = xs === join (pure xs)

prop_levelsBindAssoc :: Property
prop_levelsBindAssoc = mapSize (min 6) (\(xs :: Levels (Levels (Levels Bool))) -> (join (join xs) === join (fmap join xs)))

prop_levelsApp :: Property
prop_levelsApp = mapSize (min 7) (\(xs :: Levels Bool) (ys :: Levels Bool) -> ((,) <$> xs <*> ys) === (fmap (,) xs `ap` ys))

prop_probSemiringPlusId :: Prob -> Property
prop_probSemiringPlusId x = mempty <> x === x

prop_heapAssoc :: Property
prop_heapAssoc =
  mapSize (min 7) (\  (xs :: MH.HeapT Dist ((,) [Bool]) Bool)
                      (ys :: MH.HeapT Dist ((,) [Bool]) Bool)
                      (zs :: MH.HeapT Dist ((,) [Bool]) Bool) ->
    liftA2 (\(x,y) z -> (x,y,z)) (liftA2 (,) xs ys) zs === liftA2 (\x (y,z) -> (x,y,z)) xs (liftA2 (,) ys zs))

return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
