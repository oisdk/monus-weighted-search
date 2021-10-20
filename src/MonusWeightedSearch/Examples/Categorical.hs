-- | 

module MonusWeightedSearch.Examples.Categorical where

import Data.Monus

-- import Control.Comonad.Trans.Cofree
-- import Control.Monad.Trans.Free
import Control.Arrow ((&&&), (|||))
import Data.Functor.Compose
import Control.Comonad
import Control.Monad

type (+) = Either
type (*) = (,)
type (.) = Compose

-- newtype CofreeT f w a = CofreeT { runCofreeT :: w (a * f (CofreeT f w a)) }
-- newtype   FreeT f m a =   FreeT { runFreeT   :: m (a + f (  FreeT f w a)) }

-- type CoHeap w = CofreeT [] ((,) w)
-- type   Heap w =   FreeT ((,) w) []

newtype CofreeF f a k = CofreeF { runCofreeF :: a * f k }
newtype   FreeF f a k =   FreeF { runFreeF :: a + f k }

type CofreeTF f w a = w . CofreeF f a
type CofreeT  f w a = Fix (CofreeTF f w a)

pattern CofreeT :: w (CofreeF f a (CofreeT f w a)) -> CofreeT f w a
pattern CofreeT {runCofreeT} = Fix (Compose runCofreeT)
{-# COMPLETE CofreeT #-}

pattern FreeT :: m (FreeF f a (FreeT f m a)) -> FreeT f m a
pattern FreeT {runFreeT} = Fix (Compose runFreeT)
{-# COMPLETE FreeT #-}


type FreeTF f m a = m . FreeF f a
type FreeT  f m a = Fix (FreeTF f m a)

type CoHeapF w a = CofreeTF [] ((,) w) a
type   HeapF w a =   FreeTF ((,) w) [] a

type CoHeap w a = CofreeT [] ((,) w) a
type   Heap w a =   FreeT ((,) w) [] a

traceT :: (Comonad w, Functor f) => (w a -> b) -> (w a -> f (w a)) -> w a -> CofreeT f w b
traceT f c xs = CofreeT (CofreeF . (f &&& fmap (traceT f c) . c) <<= xs)

evalT :: (Functor f, Monad m) => (a -> m b) -> (f (m b) -> m b) -> FreeT f m a -> m b
evalT f a = (either f (a . fmap (evalT f a)) . runFreeF) <=< runFreeT

pattern Root :: w -> a -> [CoHeap w a] -> CoHeap w a
pattern Root w x xs = Fix (Compose (w, CofreeF (x, xs)))
{-# COMPLETE Root #-}

pattern RootF :: w -> a -> [CoHeap w a] -> CoHeapF w a (CoHeap w a)
pattern RootF w x xs = Compose (w, CofreeF (x, xs))
{-# COMPLETE RootF #-}

newtype Fix f = Fix { unFix :: f (Fix f) }

apo :: Functor f => (a -> f (Fix f + a)) -> a -> Fix f
apo f = Fix . fmap (id ||| apo f) . f

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

para :: Functor f => (f (Fix f * a) -> a) -> Fix f -> a
para f = f . fmap (id &&& para f) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

type NEListF w a = (,) w . CofreeF Maybe a

type Pairing w a = Maybe (CoHeap w a) -> Maybe (CoHeap w a)

pcons :: Monus w => CoHeap w a -> Pairing w a -> Pairing w a
pcons x  k Nothing   = k (Just x)
pcons x2 k (Just x1) = Just (maybe (x1 <+> x2) ((x1 <+> x2) <+>) (k Nothing))

pnil :: Pairing w a
pnil = id

prun :: Pairing w a -> Maybe (CoHeap w a)
prun xs = xs Nothing

popMin :: Monus w => CoHeap w a -> NEListF w a (CoHeap w a)
popMin = Compose . fmap  (CofreeF . fmap (prun . foldr pcons pnil) . runCofreeF). getCompose . unFix

(<+>) :: Monus w => CoHeap w a -> CoHeap w a -> CoHeap w a
Root xw x xs <+> Root yw y ys
  | xw <= yw  = Root xw x (Root (yw |-| xw) y ys : xs)
  | otherwise = Root yw y (Root (xw |-| yw) x xs : ys)
