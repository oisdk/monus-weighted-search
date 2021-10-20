-- | 

module MonusWeightedSearch.Examples.Categorical where

import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Arrow ((&&&), (|||))
import Data.Functor.Compose

type (+) = Either
type (*) = (,)
type (.) = Compose

-- newtype CofreeT f w a = CofreeT { runCofreeT :: w (a * f (CofreeT f w a)) }
-- newtype   FreeT f m a =   FreeT { runFreeT   :: m (a + f (  FreeT f w a)) }

type CoHeap w = CofreeT [] ((,) w)
type   Heap w =   FreeT ((,) w) []

type CoHeapF w a = ((,) w . CofreeF [] a)
type   HeapF w a = ([] . FreeF ((,) w) a)

-- conv :: Heap w a -> Fix (HeapF w a)
-- conv = Fix . Compose . fmap (fmap conv) . runFreeT



newtype Fix f = Fix { unFix :: f (Fix f) }

apo :: Functor f => (a -> f (Fix f + a)) -> a -> Fix f
apo f = Fix . fmap (id ||| apo f) . f

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

-- something :: (Functor f, Functor g) => (f (Fix g) -> g (Fix g + f (Fix g))) -> Fix f -> Fix g
-- something c = cata (apo c)

double :: (Functor f, Functor g) => (f (Fix f * g (Fix f)) -> g (Fix f)) -> Fix f -> Fix g
double c = ana (para c)

para :: Functor f => (f (Fix f * a) -> a) -> Fix f -> a
para f = f . fmap (id &&& para f) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

type NEListF w a = (,) w . CofreeF Maybe a

