{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Test.QuickCheck
    ( allProperties,
      forAll,
      mapSize,
      (.&&.),
      counterexample,
      (===),
      arbitrarySizedNatural,
      Arbitrary2(liftShrink2),
      Arbitrary1(..),
      Property,
      Arbitrary(..),
      Gen )
import Test.Tasty.QuickCheck ( testProperties )
import Test.Tasty ( defaultMain )

import Data.Functor.Identity ( Identity )
import Text.Read ( readEither )
import Control.Monad.Writer
    ( Endo(Endo, appEndo), MonadWriter(writer) )
import Data.List (sort)
import Data.Bifoldable ( Bifoldable(bifoldl, bifoldr) )
import Data.Foldable ( asum )
import Control.Applicative ( Applicative(liftA2) )
import Data.Word ( Word8 )
import Data.Monus.Max ( Max )
import Numeric.Natural ( Natural )
import Text.Printf

import Control.Monad.Heap ( fromList, Heap, HeapT )
import Control.Monad.Heap.List ( ListCons((:-)), ListT )

import MonusWeightedSearch.Internal.AdjList ( toGraph, AdjList )

import qualified MonusWeightedSearch.Internal.Heap as H
import qualified MonusWeightedSearch.Examples.Dijkstra as M
import qualified MonusWeightedSearch.Examples.Sort as M

import Data.Monus ( Monus(..) )
import Data.Monus.Prob ( Prob )
import Data.Monus.Dist ( Dist )

import qualified GHC.Exts as IsList
import Data.Function (on)

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = map fromInteger . filter (0<=) . shrink . toInteger

prop_monadDijkstra :: AdjList -> Property
prop_monadDijkstra gm = sort (H.dijkstra (toGraph gm) 1) === sort (M.dijkstra (toGraph gm) 1)

prop_monadSort :: [Word] -> Property
prop_monadSort xs = sort xs === M.monusSort xs

monusPositiveLaw :: (Monus a, Show a) => a -> a -> Property
monusPositiveLaw x y =
  counterexample ((disp `on` show) x y) (x <= x <> y) .&&.
  counterexample ((disp `on` show) y x) (y <= y <> x)
    where disp lhs rhs = printf "%s !<= %s <> %s" lhs lhs rhs
     
monusInvLaw :: (Show a, Monus a) => a -> a -> Property
monusInvLaw x y
  | x <= y    = counterexample ((disp `on` show) x y) (x <> (y |-| x) === y)
  | otherwise = counterexample ((disp `on` show) y x) (y <> (x |-| y) === x)
 where
   disp lhs rhs = printf "%s <> (%s |-| %s) /= %s" lhs rhs lhs rhs

monusLaws :: (Show a, Monus a) => a -> a -> Property
monusLaws x y = monusPositiveLaw x y .&&. monusInvLaw x y

prop_probMonoid :: Prob -> Prob -> Property
prop_probMonoid = monusLaws

prop_probMonus :: Prob -> Prob -> Property
prop_probMonus = monusLaws

prop_maxMonus :: Max Word8 -> Max Word8 -> Property
prop_maxMonus = monusLaws

prop_readListT :: ListT Identity Word -> Property
prop_readListT xs = readEither (show xs) === Right xs

prop_readHeapT :: HeapT Dist Identity Word -> Property
prop_readHeapT xs = readEither (show xs) === Right xs

prop_readMax :: Max Word -> Property
prop_readMax xs = readEither (show xs) === Right xs

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

prop_toFromListHeap :: [(Word, Dist)] -> Property
prop_toFromListHeap xs = IsList.toList (IsList.fromList xs :: Heap Dist Word) === xs

return []

main :: IO ()
main = defaultMain (testProperties "Properties" $allProperties)
