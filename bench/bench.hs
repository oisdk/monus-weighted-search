module Main (main) where

import Criterion.Main
import Control.Monad
import System.Random

import Data.WeightedGraph
import Data.WeightedGraph.AdjList

import Data.Dist

import qualified Control.Monad.Dijkstra as M
import qualified Control.Monad.Search as M
import qualified Data.Heap as H

onG :: (Graph Word -> Graph Word) -> AdjList -> AdjList
onG f al = fromGraph (alSize al) (f (toGraph al))
{-# INLINE onG #-}


dijkstraBench :: Word -> Benchmark
dijkstraBench n = env (randAdjList 10 n :: IO AdjList)
    $ \xs -> bgroup (show n)
      [ bench "monad" $ nf (onG M.dijkstra) xs
      , bench "heap" $ nf (onG H.dijkstra) xs
      ]

sortBench :: Int -> Benchmark
sortBench n = env (replicateM n (fmap toEnum (randomRIO (0,100000)))) $
  \xs -> bgroup (show n)
      [ bench "monad" $ nf M.dsort xs
      , bench "heap" $ nf H.dsort xs
      ]

main :: IO ()
main =
  defaultMain
    [ bgroup "sort" $ map sortBench (map (10000*) [1..10])
    , bgroup "dijkstra" $ map dijkstraBench (map (40*) [1..10]) ]
