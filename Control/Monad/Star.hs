module Control.Monad.Star where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty

star :: MonadPlus m => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)

plus :: MonadPlus m => (a -> m a) -> a -> m a
plus f x = f x >>= star f

pathed :: MonadPlus m => (a -> m a) -> a -> m (NonEmpty a)
pathed f = star (\ ~(x :| xs) -> fmap (:|x:xs) (f x)) . (:| [])
