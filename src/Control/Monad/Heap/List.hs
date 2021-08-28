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
import Data.Bifoldable
import Data.Bitraversable

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

instance Bifunctor ListCons where
  bimap f g Nil = Nil
  bimap f g (x :- xs) = f x :- g xs
  {-# INLINE bimap #-}

instance Bifoldable ListCons where
  bifold Nil = mempty
  bifold (x :- xs) = x <> xs
  {-# INLINE bifold #-}

  bifoldMap f g Nil = mempty
  bifoldMap f g (x :- xs) = f x <> g xs
  {-# INLINE bifoldMap #-}

  bifoldr f g b Nil = b
  bifoldr f g b (x :- xs) = f x (g xs b)
  {-# INLINE bifoldr #-}

  bifoldl f g b Nil = b
  bifoldl f g b (x :- xs) = g (f b x) xs
  {-# INLINE bifoldl #-}
  
instance Bitraversable ListCons where
  bitraverse f g Nil = pure Nil
  bitraverse f g (a :- b) = liftA2 (:-) (f a) (g b)
  {-# INLINE bitraverse #-}

instance Arbitrary2 ListCons where
  liftArbitrary2 xs ys = sized (\n -> frequency ((1, pure Nil) : [(n, liftA2 (:-) xs ys) | n >= 1]))
  liftShrink2 xs ys Nil = []
  liftShrink2 xs ys (x :- y) = Nil : map (uncurry (:-)) (liftShrink2 xs ys (x,y))
  
instance Arbitrary a => Arbitrary1 (ListCons a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (ListCons a b) where
  arbitrary = arbitrary2
  shrink = shrink2
  
newtype ListT m a
  = ListT { runListT :: m (ListCons a (ListT m a)) }
  deriving (Typeable, Generic)
  deriving (Semigroup, Monoid) via Alt (ListT m) a

deriving newtype instance (forall x. NFData x => NFData (m x), NFData a) => NFData (ListT m a)

unfoldrM :: Functor m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldrM f = ListT #. fmap (maybe Nil (uncurry (:-) . second (unfoldrM f))) . f
{-# INLINE unfoldrM #-}

instance (forall x. Show x => Show (m x), Show a) => Show (ListT m a) where
  showsPrec n (ListT xs) = showParen (n > 10) (showString "ListT " . showsPrec 11 xs)
  
deriving instance (forall x. Eq x => Eq (m x), Eq a) => Eq (ListT m a)
deriving instance (forall x. Ord x => Ord (m x), Eq (ListT m a), Ord a) => Ord (ListT m a)

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
  foldr f  = go
    where
      go b = foldr g b .# runListT
      
      g Nil ys = ys
      g (y :- ys) zs = f y (go zs ys)
  {-# INLINE foldr #-}

instance Traversable m => Traversable (ListT m) where
  traverse f = go
    where
      go = fmap ListT . (traverse (bitraverse f go) .# runListT)
  {-# INLINE traverse #-}
  
toListT :: Monad m => ListT m a -> m [a]
toListT = foldrListT (fmap . (:)) (pure [])
{-# INLINE toListT #-}

instance Monad m => Alternative (ListT m) where
  xs <|> ys = ListT (foldrListT (\z zs -> pure (z :- ListT zs)) (runListT ys) xs)
  {-# INLINE (<|>) #-}
  empty = ListT (pure Nil)
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
