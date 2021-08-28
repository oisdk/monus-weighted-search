module Control.Monad.Star where

import Control.Applicative
import Control.Monad

star :: MonadPlus m => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)

plus :: MonadPlus m => (a -> m a) -> a -> m a
plus f x = f x >>= star f
