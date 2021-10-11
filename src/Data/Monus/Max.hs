--------------------------------------------------------------------------------
-- |
-- Module      : Data.Monus.Max
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A 'Monus' for for maximums.
--------------------------------------------------------------------------------

module Data.Monus.Max where

import Control.Applicative
import Control.Monad

import Data.Monus

import Test.QuickCheck
import Control.DeepSeq
import Data.Functor.Classes
import Text.Read

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import GHC.Read (expectP)
import Data.Functor (($>))

-- | A type which adds a lower bound to some ordered type.
data Max a = Bot | In a
  deriving stock (Eq, Data, Generic, Typeable, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Max a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance NFData a => NFData (Max a) where
  rnf Bot = ()
  rnf (In x) = rnf x

instance Eq1 Max where
  liftEq _ Bot Bot = True
  liftEq eq (In x) (In y) = eq x y
  liftEq _ _ _ = False

instance Ord1 Max where
  liftCompare cmp Bot Bot = EQ
  liftCompare cmp Bot (In _) = LT
  liftCompare cmp (In _) Bot = GT
  liftCompare cmp (In x) (In y) = cmp x y

instance Show1 Max where
  liftShowsPrec sp sl n Bot = showString "Bot"
  liftShowsPrec sp _ d (In x) = showsUnaryWith sp "In" d x

instance Read1 Max where
  liftReadPrec rp _ =
      parens (expectP (Ident "Bot") $> Bot)
      <|>
      readData (readUnaryWith rp "In" In)

  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

instance Arbitrary1 Max where
  liftArbitrary arb = fmap (maybe Bot In) (liftArbitrary arb)
  liftShrink shr Bot = []
  liftShrink shr (In x) = Bot : fmap In (shr x)

instance Ord a => Ord (Max a) where
  Bot  <= _    = True
  In _ <= Bot  = False
  In x <= In y = x <= y

  (>=) = flip (<=)

  x < y = not (x >= y)

  (>) = flip (<)

  max = (<>)

  min = liftA2 min

  compare Bot    Bot    = EQ
  compare Bot    (In _) = LT
  compare (In _) Bot    = GT
  compare (In x) (In y) = compare x y

instance Applicative Max where
  pure = In

  liftA2 f (In x) (In y) = In (f x y)
  liftA2 _ _ _ = Bot

  In f <*> In x = In (f x)
  _ <*> _ = Bot

instance Monad Max where
  Bot  >>= _ = Bot
  In x >>= f = f x

instance Alternative Max where
  empty = Bot

  Bot <|> y = y
  x   <|> _ = x

instance MonadPlus Max

instance Ord a => Semigroup (Max a) where
  Bot  <> y = y
  In x <> ys = In (case ys of
                    Bot -> x
                    In y -> max x y)

instance Ord a => Monoid (Max a) where
  mempty = Bot
  {-# INLINE mempty #-}

instance Ord a => Monus (Max a) where
  (|-|) = (<>)
  {-# INLINE (|-|) #-}
