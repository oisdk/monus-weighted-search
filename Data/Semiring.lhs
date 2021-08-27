%include polycode.fmt
%include forall.fmt

%subst keyword a = "\textcolor{AHKeywordColour}{\textbf{" a "}}"

\begin{code}
module Data.Semiring where

import Numeric.Natural
import Data.Bits
import Data.Coerce
import Data.Coerce.Operators

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex

class Semiring a where
\end{code}
%if False
\begin{code}
  {-# MINIMAL (<+>), (<.>), ((one, zer) | fromNatural) #-}
\end{code}
%endif
\begin{code}
  infixl 6 <+>
  infixl 7 <.>

  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a

  one :: a
  one = fromNatural 1
  {-# INLINE one #-}

  zer :: a
  zer = fromNatural 0
  {-# INLINE zer #-}

  fromNatural :: Natural -> a
  fromNatural = go
    where
      two = one <+> one

      go 0 = zer
      go 1 = one
      go 2 = two
      go n
        | testBit n 0 = one <+> two <.> go (unsafeShiftR n 1)
        | otherwise   = two <.> go (unsafeShiftR n 1)
  {-# INLINE fromNatural #-}

newtype WrappedNum a = WrappedNum { runWrappedNum :: a }

instance Num a => Semiring (WrappedNum a) where
  (<+>) = (coerce :: (a -> a -> a) -> (WrappedNum a -> WrappedNum a -> WrappedNum a)) (+)
  {-# INLINE (<+>) #-}
  (<.>) = (coerce :: (a -> a -> a) -> (WrappedNum a -> WrappedNum a -> WrappedNum a)) (*)
  {-# INLINE (<.>) #-}
  fromNatural = WrappedNum #. fromInteger . toInteger
  {-# INLINE fromNatural #-}
  one = WrappedNum (fromInteger 1)
  {-# INLINE one #-}

  zer = WrappedNum (fromInteger 0)
  {-# INLINE zer #-}

instance Semiring Natural where
  (<+>) = (+)
  {-# INLINE (<+>) #-}

  (<.>) = (*)
  {-# INLINE (<.>) #-}

  one = 1
  {-# INLINE one #-}

  zer = 0
  {-# INLINE zer #-}

  fromNatural = id
  {-# INLINE fromNatural #-}

instance Semiring Integer where
  (<+>) = (+)
  {-# INLINE (<+>) #-}

  (<.>) = (*)
  {-# INLINE (<.>) #-}

  one = 1
  {-# INLINE one #-}

  zer = 0
  {-# INLINE zer #-}

  fromNatural = toInteger
  {-# INLINE fromNatural #-}

instance Semiring Bool where
  (<+>) = (||)
  {-# INLINE (<+>) #-}

  (<.>) = (&&)
  {-# INLINE (<.>) #-}

  one = True
  {-# INLINE one #-}

  zer = False
  {-# INLINE zer #-}

  fromNatural = (1<=)
  {-# INLINE fromNatural #-}

deriving via WrappedNum Float instance Semiring Float
deriving via WrappedNum Double instance Semiring Double

deriving via WrappedNum Int instance Semiring Int
deriving via WrappedNum Int8 instance Semiring Int8
deriving via WrappedNum Int16 instance Semiring Int16
deriving via WrappedNum Int32 instance Semiring Int32
deriving via WrappedNum Int64 instance Semiring Int64

deriving via WrappedNum Word instance Semiring Word
deriving via WrappedNum Word8 instance Semiring Word8
deriving via WrappedNum Word16 instance Semiring Word16
deriving via WrappedNum Word32 instance Semiring Word32
deriving via WrappedNum Word64 instance Semiring Word64

deriving via WrappedNum (Ratio a) instance Integral a => Semiring (Ratio a)
deriving via WrappedNum (Complex a) instance RealFloat a => Semiring (Complex a)

instance Semiring b => Semiring (a -> b) where
  (f <+> g) x = f x <+> g x
  (f <.> g) x = f x <.> g x
  one _ = one
  zer _ = zer
  fromNatural n _ = fromNatural n
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE one #-}
  {-# INLINE zer #-}
  {-# INLINE fromNatural #-}

instance (Semiring a, Semiring b) => Semiring (a,b) where
  ~(xl,xr) <+> ~(yl,yr) = (xl <+> yl, xr <+> yr)
  ~(xl,xr) <.> ~(yl,yr) = (xl <.> yl, xr <.> yr)
  one = (one, one)
  zer = (zer, zer)

  fromNatural n = (fromNatural n, fromNatural n)

  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE one #-}
  {-# INLINE zer #-}
  {-# INLINE fromNatural #-}
\end{code}
