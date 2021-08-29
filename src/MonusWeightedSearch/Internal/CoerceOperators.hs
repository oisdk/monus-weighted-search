-- |
-- Module      : MonusWeightedSearch.Internal.CoerceOperators
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Some utility operators for writing coercions.
-- Taken from the profunctors package.

module MonusWeightedSearch.Internal.CoerceOperators where

import Data.Coerce

-- $setup
-- >>> import Data.Functor.Identity

infixr 9 #.
-- | Compose a function with a no-op on the left.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
_ #. f = coerce f
{-# INLINE (#.) #-}

infixr 9 .#
-- | Compose a function with a no-op on the right.
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
f .# _ = coerce f
{-# INLINE (.#) #-}

infixl 4 <#$>
-- | 'fmap' with a no-op.
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce
{-# INLINE (<#$>) #-}

-- | Coerce a binary operator to work "under" a newtype.
--
-- >>> ((+) `under` runIdentity) (Identity 3) (Identity 5)
-- Identity 8
under :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
under f _ = coerce f
{-# INLINE under #-}
