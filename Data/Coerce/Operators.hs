module Data.Coerce.Operators where

import Data.Coerce

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
_ #. f = coerce f
{-# INLINE (#.) #-}

infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
f .# _ = coerce f
{-# INLINE (.#) #-}

infixl 4 <#$>
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce
{-# INLINE (<#$>) #-}

under :: Coercible a b => (b -> b -> b) -> (a -> b) -> a -> a -> a
under f _ = coerce f
{-# INLINE under #-}
