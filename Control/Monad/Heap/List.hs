{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Heap.List
  ( -- * Type Definition
    ListCons(..)
  , ListT(..)
    -- * Building the list
  , unfoldrM
    -- * Running the list
  , toListT
    -- * Transforming lists
  , catMaybesT
  , listMmap
  ) where

import Data.Bifunctor
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Test.QuickCheck
import MonusWeightedSearch.Internal.CoerceOperators
import Data.Functor.Classes
import Control.DeepSeq
import GHC.Generics
import Data.Data

infixr 5 :-
data ListCons a b = Nil | a :- !b
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)
-- ^The list constructor

instance (NFData a, NFData b) => NFData (ListCons a b) where
  rnf Nil = ()
  rnf (x :- xs) = rnf x `seq` rnf xs
  {-# INLINE rnf #-}

instance Eq2 ListCons where
  liftEq2 lhs rhs Nil Nil = True
  liftEq2 lhs rhs (x :- xs) (y :- ys) = lhs x y && rhs xs ys
  liftEq2 _ _ _ _ = False
  {-# INLINE liftEq2 #-}

instance Eq a => Eq1 (ListCons a) where
  liftEq = liftEq2 (==)
  {-# INLINE liftEq #-}
  
instance Ord2 ListCons where
  liftCompare2 lhs rhs Nil Nil = EQ
  liftCompare2 lhs rhs (x :- xs) (y :- ys) = lhs x y <> rhs xs ys
  liftCompare2 _ _ Nil _ = LT
  liftCompare2 _ _ _ Nil = GT
  {-# INLINE liftCompare2 #-}
  
instance Ord a => Ord1 (ListCons a) where
  liftCompare = liftCompare2 compare
  {-# INLINE liftCompare #-}

instance Bifunctor ListCons where
  bimap f g Nil = Nil
  bimap f g (x :- xs) = f x :- g xs
  {-# INLINE bimap #-}
  
newtype ListT m a
  = ListT { runListT :: m (ListCons a (ListT m a)) }
  deriving (Typeable, Generic)
-- ^The list monad transformer

instance (forall x. NFData x => NFData (m x), NFData a) => NFData (ListT m a) where
  rnf = rnf .# runListT
  {-# INLINE rnf #-}
  
unfoldrM :: Functor m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldrM f = ListT #. fmap (maybe Nil (uncurry (:-) . second (unfoldrM f))) . f
{-# INLINE unfoldrM #-}

instance (forall x. Show x => Show (m x), Show a) => Show (ListT m a) where
  showsPrec n (ListT xs) = showParen (n > 10) (showString "ListT " . showsPrec 11 xs)
  
instance Eq1 m => Eq1 (ListT m) where
  liftEq eq (ListT xs) (ListT ys) = liftEq (liftEq2 eq (liftEq eq)) xs ys
  {-# INLINE liftEq #-}
      
instance Ord1 m => Ord1 (ListT m) where
  liftCompare cmp (ListT xs) (ListT ys) = liftCompare (liftCompare2 cmp (liftCompare cmp)) xs ys
  {-# INLINE liftCompare #-}

instance (Eq1 m, Eq a) => Eq (ListT m a) where
  (==) = eq1
  {-# INLINE (==) #-}
instance (Ord1 m, Ord a) => Ord (ListT m a) where
  compare = compare1
  {-# INLINE compare #-}

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (ListT m a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary1 m => Arbitrary1 (ListT m) where
  liftArbitrary arb = sized go
    where
      go n
        | n <= 0    = ListT <#$> liftArbitrary (pure Nil)
        | otherwise = ListT <#$> liftArbitrary (liftA2 (:-) arb (go (n-1)))
  liftShrink shr = map ListT #. (liftShrink f .# runListT)
    where
      f Nil       = []
      f (x :- xs) = Nil : map (uncurry (:-)) (liftShrink2 shr (liftShrink shr) (x,xs))
  
instance Functor m => Functor (ListT m) where
  fmap f = ListT #. (fmap (bimap f (fmap f)) .# runListT)
  {-# INLINE fmap #-}
  
foldrListT :: Monad m => (a -> m b -> m b) -> m b -> ListT m a -> m b
foldrListT f b = (>>= h) .# runListT
  where
    h Nil = b
    h (x :- ListT xs) = f x (xs >>= h)
{-# INLINE foldrListT #-}

instance Monad m => Semigroup (ListT m a) where
  xs <> ys = ListT (foldrListT (\z zs -> pure (z :- ListT zs)) (runListT ys) xs)
  {-# INLINE (<>) #-}
  
instance Monad m => Monoid (ListT m a) where
  mempty = ListT (pure Nil)
  {-# INLINE mempty #-}
  
instance Monad m => Applicative (ListT m) where
  pure x = ListT (pure (x :- ListT (pure Nil)))
  {-# INLINE pure #-}
  liftA2 f xs ys = ListT (foldrListT g (pure Nil) xs)
    where
      g x xs = foldrListT (h x) xs ys
      h x y xs = pure (f x y :- ListT xs)
  {-# INLINE liftA2 #-}
  (*>) = liftA2 (const id)
  {-# INLINE (*>)#-}
  (<*) = liftA2 const
  {-# INLINE (<*) #-}
  
instance Monad m => Monad (ListT m) where
  xs >>= f = ListT (foldrListT g (pure Nil) xs)
    where
      g x xs = runListT (f x <> ListT xs)
  {-# INLINE (>>=) #-}
  
instance Foldable m => Foldable (ListT m) where
  foldr f b = foldr g b .# runListT
    where
      g Nil ys = ys
      g (y :- ys) zs = f y (foldr f zs ys)
  {-# INLINE foldr #-}
  
toListT :: Monad m => ListT m a -> m [a]
toListT = foldrListT (fmap . (:)) (pure [])
{-# INLINE toListT #-}

instance Monad m => Alternative (ListT m) where
  (<|>) =(<>)
  {-# INLINE (<|>) #-}
  empty = mempty
  {-# INLINE empty #-}
  
instance Monad m => MonadPlus (ListT m)

instance MonadTrans ListT where
  lift = ListT . fmap (\x -> x :- ListT (pure Nil))
  {-# INLINE lift #-}
  
listMmap :: Functor m => (m (ListCons a (ListT n b)) -> n (ListCons b (ListT n b))) -> ListT m a -> ListT n b
-- ^ Apply a function to every effect layered in the list transformer
listMmap f = ListT #. f . (fmap (fmap (listMmap f)) .# runListT)
{-# INLINE listMmap #-}

instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}
  
instance MonadError e m => MonadError e (ListT m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError xs h = listMmap (`catchError` (runListT . h)) xs
  {-# INLINE catchError #-}
  
instance MonadReader r m => MonadReader r (ListT m) where
  ask = lift ask
  {-# INLINE ask #-}
  reader = lift . reader
  {-# INLINE reader #-}
  local = listMmap . local
  {-# INLINE local #-}
  
instance MonadWriter w m => MonadWriter w (ListT m) where
  writer = lift . writer
  {-# INLINE writer #-}
  tell = lift . tell
  {-# INLINE tell #-}
  listen = listMmap (fmap (uncurry (flip (first . flip (,)))) . listen)
  {-# INLINE listen #-}
  pass = listMmap (pass . fmap h)
    where
      h Nil = (Nil, id)
      h ((x,f):-xs) = (x :- xs , f)
  {-# INLINE pass #-}
      
instance MonadCont m => MonadCont (ListT m) where
  callCC f = ListT (callCC (\c -> runListT (f (ListT . c . (:- empty)))))
  {-# INLINE callCC #-}
  
catMaybesT :: Monad m => (a -> Maybe b) -> ListT m a -> ListT m b
catMaybesT f xs = xs >>= (maybe empty pure . f)
{-# INLINE catMaybesT #-}
