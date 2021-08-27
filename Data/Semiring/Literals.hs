{-# LANGUAGE RebindableSyntax #-}

module Data.Semiring.Literals where

import Prelude hiding (fromInteger)
import qualified Prelude

import Data.Semiring

fromInteger :: Semiring a => Integer -> a
fromInteger = fromNatural . Prelude.fromInteger
{-# INLINE fromInteger #-}
