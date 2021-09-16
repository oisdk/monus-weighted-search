{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Heap.List
-- Copyright   : (c) Donnacha Oisín Kidney 2021
-- Maintainer  : mail@doisinkidney.com
-- Stability   : experimental
-- Portability : non-portable
--
-- A simple re-implementation of the list monad transformer, for use by the heap
-- monad.
--------------------------------------------------------------------------------
module Control.Monad.Heap.List
  ( -- * Type Definition
    ListT(..)
  , ListCons(..)
    -- * Building the list
  , unfoldrM
    -- * Running the list
  , toListT
    -- * Transforming lists
  , catMaybesT
  , listMmap
  ) where

import Data.Bifunctor ( Bifunctor(first, bimap) )
import Data.Bifoldable ( Bifoldable(..) , bifoldl', bifoldr')
import Data.Bitraversable ( Bitraversable(..) )
import Control.Monad ( MonadPlus )
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>), empty) )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.State ( MonadState(..) )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.Reader ( MonadReader(..) )
import Control.Monad.Writer ( Alt(Alt), MonadWriter(..) )
import Control.Monad.Cont ( MonadCont(..) )
import Test.QuickCheck
    ( Arbitrary(..),
      arbitrary1,
      arbitrary2,
      shrink1,
      shrink2,
      frequency,
      sized,
      Arbitrary1(..),
      Arbitrary2(..) )
import MonusWeightedSearch.Internal.CoerceOperators
    ( (#.), (.#), (<#$>) )
import Control.DeepSeq ( NFData(..) )
import GHC.Generics ( Generic, Generic1 )
import Data.Data ( Data, Typeable )
import Data.Coerce ( coerce )
import Data.Foldable ( Foldable(foldr', foldl') )
import Text.Read (readPrec, parens, prec, Lexeme(Ident), lexP, step)

infixr 5 :-
-- | The list constructor.
data ListCons a b = Nil | a :- !b
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

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

-- | A list monad transformer, "done right".
newtype ListT m a
  = ListT { runListT :: m (ListCons a (ListT m a)) }
  deriving (Typeable, Generic)
  deriving (Semigroup, Monoid) via Alt (ListT m) a

deriving instance (forall x. Data x => Data (m x), Typeable m, Data a) => Data (ListT m a)

deriving newtype instance (forall x. NFData x => NFData (m x), NFData a) => NFData (ListT m a)

-- | Unfold, monadically, a list from a seed.
unfoldrM :: Functor m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldrM f = go
  where
    go = ListT #. fmap h . f

    h Nothing = Nil
    h (Just (x, xs)) = x :- go xs
    {-# INLINE h #-}
{-# INLINE unfoldrM #-}

instance (forall x. Show x => Show (m x), Show a) => Show (ListT m a) where
  showsPrec n (ListT xs) = showParen (n > 10) (showString "ListT " . showsPrec 11 xs)
  
instance (forall x. Read x => Read (m x), Read a) => Read (ListT m a) where
  readPrec = parens $
      prec 10 $ do
        Ident "ListT" <- lexP
        m <- step readPrec
        return (ListT m)
  
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
  {-# INLINE (*>) #-}
  (<*) = liftA2 const
  {-# INLINE (<*) #-}
  
instance Monad m => Monad (ListT m) where
  xs >>= f = ListT (foldrListT g (pure Nil) xs)
    where
      g x xs = runListT (f x <> ListT xs)
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  
instance Foldable m => Foldable (ListT m) where
  foldr f = go
    where
      go = (. runListT) #. foldr (flip (bifoldr f (flip go)))
  {-# INLINE foldr #-}
  
  foldMap f = go
    where
      go = foldMap (bifoldMap f go) .# runListT
  {-# INLINE foldMap #-}
  foldl f = go
    where
      go = (. runListT) #. foldl (bifoldl f go)
  {-# INLINE foldl #-}
  
  foldl' f = go
    where
      go = (. runListT) #. foldl' (bifoldl' f go)
  {-# INLINE foldl' #-}
  
  foldr' f = go
    where
      go = (. runListT) #. foldr' (flip (bifoldr' f (flip go)))
  {-# INLINE foldr' #-}

instance Traversable m => Traversable (ListT m) where
  traverse f = fmap ListT . (traverse h .# runListT)
    where
      h Nil = pure Nil
      h (x :- ListT xs) = liftA2 ((. ListT) #. (:-)) (f x) (traverse h xs)
  {-# INLINE traverse #-}

-- | Flatten all of the effects in the list and collect the results.
toListT :: Monad m => ListT m a -> m [a]
toListT = foldrListT (fmap . (:)) (pure [])
{-# INLINE toListT #-}

instance Monad m => Alternative (ListT m) where
  (<|>) = (coerce :: (ListT m a -> m (ListCons a (ListT m a)) -> m (ListCons a (ListT m a)))
                  -> (ListT m a -> ListT m a -> ListT m a))
          (flip (foldrListT (\z zs -> pure (z :- ListT zs))))
  {-# INLINE (<|>) #-}
  empty = ListT (pure Nil)
  {-# INLINE empty #-}
  
instance Monad m => MonadPlus (ListT m)

instance MonadTrans ListT where
  lift = ListT #. fmap (:- ListT (pure Nil))
  {-# INLINE lift #-}
  
-- | Apply a function to every effect layered in the list transformer.
listMmap :: Functor m
         => (m (ListCons a (ListT n b)) -> n (ListCons b (ListT n b)))
         -> ListT m a -> ListT n b
listMmap f = go
  where
    go = ListT #. f . (fmap (fmap go) .# runListT)
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
  callCC f = ListT (callCC (\c -> runListT (f (ListT #. c . (:- empty)))))
  {-# INLINE callCC #-}

-- | Filter the list. An analogue of mapMaybe on lists.
catMaybesT :: Monad m => (a -> Maybe b) -> ListT m a -> ListT m b
catMaybesT f = ListT #. foldrListT (\x xs -> maybe xs (pure . (:- ListT xs)) (f x)) (pure Nil)
{-# INLINE catMaybesT #-}
