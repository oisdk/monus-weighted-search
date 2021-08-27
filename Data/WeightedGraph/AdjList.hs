module Data.WeightedGraph.AdjList
  ( AdjList(..)
  , randAdjList
  , alSize
  , toGraph
  , fromGraph
  ) where

import Data.WeightedGraph
import Data.Array.Unboxed
import Control.DeepSeq
import Test.QuickCheck
import Data.List
import MonusWeightedSearch.Internal.CoerceOperators
import MonusWeightedSearch.Internal.TestHelpers
import System.Random
import Data.Bool

newtype AdjList
  = AdjList
  { edges :: UArray (Word,Word) Word
  } deriving (Eq, Ord)

instance NFData AdjList where
  rnf = flip seq () .# edges
  {-# INLINE rnf #-}

edgeDensity :: Word
-- ^The default edge density of randomly-generated graphs.
edgeDensity = 50

randAdjList :: Word -- ^ Edge Density (as a percentage)
            -> Word -- ^ Size
            -> IO AdjList
randAdjList edgeDensity n =
      AdjList #. array ((0,0),(n-1,n-1))
         <$> sequence [ sequence ((i,j), edge)
                      | i <- [0..n-1], j <- [0..n-1], i /= j ]
  where
    edge :: IO Word
    edge = percentageChanceIO edgeDensity >>= bool (pure 0) (randomRIO (1, 30))
    {-# INLINE edge #-}
{-# INLINE randAdjList #-}

instance Arbitrary AdjList where
  arbitrary = sized (go . succ . toEnum)
    where
      go :: Word -> Gen AdjList
      go n = AdjList #. array ((0,0),(n-1,n-1))
         <$> sequence [ sequence ((i,j), percentageChance edgeDensity >>= bool (pure 0) (fmap succ arbitrary))
                      | i <- [0..n-1], j <- [0..n-1], i /= j ]
  shrink xs = take (fromEnum (alSize xs) - 1) (iterate cut (cut xs))
    where
      cut ar = AdjList (listArray ((0,0),(s,s)) (map (edges ar !) (range ((0,0),(s,s)))))
        where s = alSize ar - 2


alSize :: AdjList -> Word
alSize = succ . snd . snd . (bounds .# edges)
{-# INLINE alSize #-}

toGraph :: AdjList -> Graph Word
toGraph (AdjList xs) i
  | i <= snd (snd (bounds xs)) =
    [ (j,toEnum (fromEnum d))
    | j <- [0..snd (snd (bounds xs))], let d = xs ! (i,j), d /= 0 ]
  | otherwise = []
{-# INLINE toGraph #-}

fromGraph :: Word -> Graph Word -> AdjList
fromGraph n g =
  AdjList (array ((0,0),(n-1,n-1)) [ ((i,j),toEnum (fromEnum d)) | i <- [0..n-1], (j,d) <- g i ])
{-# INLINE fromGraph #-}

instance Show AdjList where
  show al = unlines (top : "│             │" : intercalate [sep] [ go s (toGraph al s)  | s <- [0..alSize al - 1] ] ++ [bot])
    where
      top = '┌' : showPad (0 :: Word,alSize al - 1) " ────────────┐"
      bot = "└─────────────┘"
      sep = "├╌╌╌╌╌╌╌╌╌╌╌╌╌┤"

      go s []     = ["│ " ++ showPad s " ──────> [] │"]
      go s ((x,xw):xs) = ("│ " ++ showPad s (" ──" ++ showPadR 3 '─' xw ++ "─> " ++ showPad x "   │")) : go' xs

      go' [] = []
      go' [(x,xw)] = ["│ └──" ++ showPadR 3 '─' xw ++ "─> " ++ showPad x "   │"]
      go' ((x,xw):xs) = ("│ ├──" ++ showPadR 3 '─' xw ++ "─> " ++ showPad x "   │") : go' xs

      tail' [] = []
      tail' (_:xs) = xs

      showPad v = foldr f id (show v)
        where
          f x k ys = x : k (tail' ys)

      showPadR n c v = let x = show v in replicate (n - length x) c ++ x
