{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Heap
  ( -- * Type definition
    Node(..)
  , HeapT(..)
    -- * Non-transformer form
  , Heap
  , pattern Heap
  , runHeap

    -- * Popping the smallest element
  , popMin
  , popMinT

    -- * Turning into a cons-list
  , flatten
  , flattenT

    -- * Searching the whole heap
  , search
  , searchT

    -- * Returning one element
  , best
  , bestT
  )
  where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Control.Monad.Heap.List
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Data.Monus
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Cont
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Test.QuickCheck

import MonusWeightedSearch.Internal.CoerceOperators
import MonusWeightedSearch.Internal.TestHelpers
import Data.Data
import GHC.Generics
import Control.DeepSeq

infixr 5 :<
data Node w a b = Leaf a | !w :< b
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Data, Typeable, Generic, Generic1)

instance (NFData w, NFData a, NFData b) => NFData (Node w a b) where
  rnf (Leaf x) = rnf x
  rnf (x :< xs) = rnf x `seq` rnf xs
  {-# INLINE rnf #-}

instance Bifunctor (Node w) where
  bimap f g (Leaf x) = Leaf (f x)
  bimap f g (x :< xs) = x :< g xs

  first f (Leaf x) = Leaf (f x)
  first f (x :< xs) = x :< xs

  second f (Leaf x) = Leaf x
  second f (x :< xs) = x :< f xs

  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifoldable (Node w) where
  bifold (Leaf x) = x
  bifold (_ :< x) = x
  {-# INLINE bifold #-}

  bifoldMap f _ (Leaf x) = f x
  bifoldMap _ f (_ :< x) = f x
  {-# INLINE bifoldMap #-}

  bifoldr f _ b (Leaf x) = f x b
  bifoldr _ f b (_ :< x) = f x b
  {-# INLINE bifoldr #-}

  bifoldl f _ b (Leaf x) = f b x
  bifoldl _ f b (_ :< x) = f b x
  {-# INLINE bifoldl #-}

instance Bitraversable (Node w) where
  bitraverse f _ (Leaf x) = fmap Leaf (f x)
  bitraverse _ f (x :< xs) = fmap (x :<) (f xs)
  {-# INLINE bitraverse #-}

newtype HeapT w m a = HeapT { runHeapT :: ListT m (Node w a (HeapT w m a)) }
  deriving (Typeable, Generic)
  deriving (Semigroup, Monoid) via Alt (HeapT w m) a

instance Foldable m => Foldable (HeapT w m) where
  foldr f = flip go
    where
      go = flip (foldr (flip (bifoldr f go))) .# runHeapT
  {-# INLINE foldr #-}

instance Traversable m => Traversable (HeapT w m) where
  traverse f = go
    where
      go = fmap HeapT . (traverse (bitraverse f go) .# runHeapT)
  {-# INLINE traverse #-}

deriving newtype instance (forall x. NFData x => NFData (m x), NFData w, NFData a) => NFData (HeapT w m a) 

instance (Arbitrary1 m, Arbitrary w, Arbitrary a) => Arbitrary (HeapT w m a) where
  arbitrary = arbitrary1

instance (Arbitrary1 m, Arbitrary w) => Arbitrary1 (HeapT w m) where
  liftArbitrary arb = sized go1
    where
      go1 n = HeapT <#$> (sumsTo n >>= foldr go2f go2b)
      go2b      = ListT <#$> liftArbitrary (pure Nil)
      go2f n ns = ListT <#$> liftArbitrary (liftA2 (:-) (go3 n) ns)
      go3 n | n <= 1 = fmap Leaf arb
      go3 n = frequency [(1, fmap Leaf arb), (n, liftA2 (:<) arbitrary (go1 n))]

type Heap w = HeapT w Identity

runHeapIdent :: Heap w a -> [Node w a (Heap w a)]
runHeapIdent = runIdentity #. (toListT .# runHeapT)
{-# INLINE runHeapIdent #-}

toHeapIdent :: [Node w a (Heap w a)] -> Heap w a
toHeapIdent = HeapT #. foldr (\x xs -> ListT (Identity (x :- xs))) (ListT (Identity Nil))
{-# INLINE toHeapIdent #-}

pattern Heap :: [Node w a (Heap w a)] -> Heap w a
pattern Heap { runHeap } <- (runHeapIdent -> runHeap)
  where
    Heap = toHeapIdent
{-# COMPLETE Heap #-}

instance (forall x. Show x => Show (m x), Show a, Show w) => Show (HeapT w m a) where
  showsPrec n (HeapT xs) = showParen (n > 10) (showString "HeapT " . showsPrec 11 xs)
  
deriving instance (forall x. Eq x => Eq (m x), Eq a, Eq w) => Eq (HeapT w m a)
-- Some special incantations are needed to make this work:
-- In my mind, the following *should* work:
-- @
-- deriving instance (Ord w, Ord a, forall x. Ord x => Ord (m x)) => Ord (HeapT w m a)
-- @
-- But for reasons described here
-- https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html#language
-- You need the following slightly more complicated thing:
deriving instance ( Ord w, Ord a
                  , forall x. Ord x => Ord (m x)
                  , Eq (HeapT w m a)                       -- These two are needed
                  , Eq (ListT m (Node w a (HeapT w m a)))  -- for reasons I do not understand!
                  ) => Ord (HeapT w m a)

instance Functor m => Functor (HeapT w m) where
  fmap f = HeapT #. (fmap (bimap f (fmap f)) .# runHeapT)
  {-# INLINE fmap #-}

instance Monad m => Applicative (HeapT w m) where
  pure = HeapT #. pure . Leaf
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  (*>) = (>>)  -- We have to do this because the default definition
               -- is (x *> y) = (id <$ x) <*> y. (which is horrifically slow)
  {-# INLINE (*>) #-}

instance Monad m => Monad (HeapT w m) where
  HeapT m >>= f = HeapT (m >>= g)
    where
      g (Leaf x) = runHeapT (f x)
      g (w :< xs) = pure (w :< (xs >>= f))
  {-# INLINE (>>=) #-}
  xs >> ys = xs >>= const ys
  {-# INLINE (>>) #-}

instance Monad m => Alternative (HeapT w m) where
  HeapT xs <|> HeapT ys = HeapT (xs <|> ys)
  empty = HeapT empty
  {-# INLINE empty #-}

instance Monad m => MonadPlus (HeapT w m)

instance MonadTrans (HeapT w) where
  lift = HeapT #. ListT #. fmap ((:- empty) . Leaf)
  {-# INLINE lift #-}

(<||>) ::  (Monus w, Monad m) =>
           (w, HeapT w m a) ->
           (w, HeapT w m a) ->
           (w, HeapT w m a)
(x, xv) <||> (y, yv)
  | x <= y    = (x, HeapT (ListT (pure ((x |-| y :< yv) :- runHeapT xv))))
  | otherwise = (y, HeapT (ListT (pure ((x |-| y :< xv) :- runHeapT yv))))
{-# INLINE (<||>) #-}

comb ::  (Monus w, Monad m) =>
         [(w, HeapT w m a)] ->
         Maybe (w, HeapT w m a)
comb [] = Nothing
comb (x:xs) = Just (comb1 x xs)
  where
    comb1 x [] = x
    comb1 x1 [x2] = x1 <||> x2
    comb1 x1 (x2 : x3 : xs) = (x1 <||> x2) <||> comb1 x3 xs
{-# INLINE comb #-}

partition :: [Node w a b] -> ([a], [(w, b)])
partition = foldr f ([],[])
  where
    f (Leaf x) (ys,zs) = (x:ys,zs)
    f (w :< x) (ys,zs) = (ys, (w, x) :zs)
{-# INLINE partition #-}

popMinT ::  (Monus w, Monad m) =>
            HeapT w m a ->
            m ([a], Maybe (w, HeapT w m a))
popMinT = fmap (second comb . partition) . toListT .# runHeapT
{-# INLINE popMinT #-}

popMin :: Monus w => Heap w a -> ([a], Maybe (w, Heap w a))
popMin = runIdentity #. popMinT
{-# INLINE popMin #-}

flattenT :: (Monad m, Monus w) => HeapT w m a -> ListT m (w, [a])
flattenT = ListT #. fmap (uncurry (:-) . bimap (mempty,) go) . popMinT
  where
    go = maybe empty (\(w, xs) -> ListT (fmap (uncurry (:-) . bimap (w,) go) (popMinT xs)))
{-# INLINE flattenT #-}

flatten :: Monus w => Heap w a -> [(w, [a])]
flatten = runIdentity #. toListT . flattenT
{-# INLINE flatten #-}

searchT ::  (Monad m, Monus w) =>
            HeapT w m a -> m [(a, w)]
searchT xs = popMinT xs >>= go mempty where
    go !w1 (x, Nothing)        = pure  (map (,w1) x)
    go !w1 (x, Just (w2, xs))  = fmap  (map (,w1) x ++) (popMinT xs >>= go (w1 <> w2))
{-# INLINE searchT #-}

search :: Monus w => Heap w a -> [(a, w)]
search = runIdentity #. searchT
{-# INLINE search #-}

bestT :: (Monad m, Monus w) => HeapT w m a -> m (Maybe a)
bestT = runMaybeT . go
  where
    go xs = do
      (y,ys) <- lift (popMinT xs)
      case y of
        z:_ -> pure z
        [] -> do
          (w', zs) <- MaybeT (pure ys)
          go zs
{-# INLINE bestT #-}
best :: Monus w => Heap w a -> Maybe a
best = runIdentity #. bestT
{-# INLINE best #-}

heapMmap :: forall m1 m2 a1 a2 w1 w2. Functor m1 =>
            (m1 (ListCons (Node w1 a1 (HeapT w2 m2 a2)) (ListT m2 (Node w2 a2 (HeapT w2 m2 a2)))) ->
             m2 (ListCons (Node w2 a2 (HeapT w2 m2 a2)) (ListT m2 (Node w2 a2 (HeapT w2 m2 a2)))))
         -> HeapT w1 m1 a1 -> HeapT w2 m2 a2
heapMmap h = goH
  where
    goH :: HeapT w1 m1 a1 -> HeapT w2 m2 a2
    goH = HeapT #. (goL .# runHeapT)
    {-# INLINE goH #-}

    goL :: ListT m1 (Node w1 a1 (HeapT w1 m1 a1)) -> ListT m2 (Node w2 a2 (HeapT w2 m2 a2))
    goL = ListT #. h . (fmap (bimap (fmap goH) goL) .# runListT)
    {-# INLINE goL #-}
{-# INLINE heapMmap #-}

instance (Monad m, Monus w) => MonadWriter w (HeapT w m) where
  writer (x, !w) = HeapT (pure (w :< pure x))
  {-# INLINE writer #-}
  tell !w = HeapT (pure (w :< pure ()))
  {-# INLINE tell #-}
  listen = go mempty
    where
      go !w = HeapT . fmap (h w) . runHeapT
      h !w1 (Leaf x) = Leaf (x, w1)
      h !w1 (w2 :< xs) = w2 :< go (w1 <> w2) xs
  {-# INLINE listen #-}
  pass = HeapT . catMaybesT (fmap (uncurry (:<)) . comb . uncurry (\w -> map (\(x,f) -> (f w, pure x)))) . flattenT
  {-# INLINE pass #-}

instance MonadState s m => MonadState s (HeapT w m) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance MonadError e m => MonadError e (HeapT w m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError xs h = heapMmap (`catchError` (runListT . runHeapT . h)) xs
  {-# INLINE catchError #-}

instance MonadReader r m => MonadReader r (HeapT w m) where
  ask = lift ask
  {-# INLINE ask #-}
  reader = lift . reader
  {-# INLINE reader #-}
  local = heapMmap . local
  {-# INLINE local #-}

instance MonadCont m => MonadCont (HeapT w m) where
  callCC f = HeapT (callCC (\c -> runHeapT (f (HeapT #. c . Leaf))))
  {-# INLINE callCC #-}
