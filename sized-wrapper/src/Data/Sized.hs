{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module        : Data.Sized
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- Create Sized version of any container.
module Data.Sized
  ( -- * Base type
    Sized,
    getSized,
    trustedSized,
    unknownSized,
    Unknown,
    Between,
    Exactly,
    AtLeast,
    AtMost,

    -- * Singleton constructor
    SizedSingleton (..),
    singleton,
    MkSizedSingletonApplicative (..),

    -- * From container
    Size (..),
    SizedFromContainer (..),
    MkSizedFromContainerFoldable (..),

    -- * Operations
    overSized,
    overSized2,
    overSized3,
    overSized4,
    overSized5,
    fmapSized,
    withSized,
    precise,
    approximate,
    (<<>>),
    withSizedAppend,
  )
where

import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy
import GHC.TypeLits
import Data.Foldable(toList)
import Data.Ix(inRange)

-- | Sized proofed value.
newtype Sized s a = Sized
  { -- | Extract the Sized proven value
    getSized :: a
  }
  deriving stock (Eq, Ord, Show)

-- * Different kind of sizes
-- | Unknown/any size
data Unknown

-- | Exactly 'n'
data Exactly (n :: Nat)

-- | At least 'n'
data AtLeast (n :: Nat)

-- | At most 'n'
data AtMost (n :: Nat)

-- | Between (included) '(n..m)'
data Between (n :: Nat) (m :: Nat)

-- * Operations

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized :: (a -> b) -> Sized s a -> Sized s b
overSized f = trustedSized . f . getSized
{-# INLINE overSized #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized2 :: (a -> b -> c) -> Sized s a -> Sized s b -> Sized s c
overSized2 f a = trustedSized . f (getSized a) . getSized
{-# INLINE overSized2 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized3 :: (a -> b -> c -> d) -> Sized s a -> Sized s b -> Sized s c -> Sized s d
overSized3 f a b = trustedSized . f (getSized a) (getSized b) . getSized
{-# INLINE overSized3 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized4 :: (a -> b -> c -> d -> e) -> Sized s a -> Sized s b -> Sized s c -> Sized s d -> Sized s e
overSized4 f a b c = trustedSized . f (getSized a) (getSized b) (getSized c) . getSized
{-# INLINE overSized4 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized5 :: (a -> b -> c -> d -> e -> f) -> Sized s a -> Sized s b -> Sized s c -> Sized s d -> Sized s e -> Sized s f
overSized5 f a b c d = trustedSized . f (getSized a) (getSized b) (getSized c) (getSized d) . getSized
{-# INLINE overSized5 #-}

-- | 'fmap' over a 'Sized' container
fmapSized :: Functor f => (a -> b) -> Sized s (f a) -> Sized s (f b)
fmapSized f = overSized (fmap f)
{-# INLINE fmapSized #-}

-- | Apply an unsafe function over empty, which is safe over 'Sized'
withSized :: (a -> Maybe b) -> Sized s a -> b
withSized f = fromJust . f . getSized
{-# INLINE withSized #-}

-- | Trusted value
trustedSized :: a -> Sized s a
trustedSized = Sized
{-# INLINE trustedSized #-}

-- | Unknown value
unknownSized :: a -> Sized Unknown a
unknownSized = Sized
{-# INLINE unknownSized #-}

-- | Singleton constructible value
class SizedSingleton a where
  type SizedSingletonElement a :: Type
  sizedSingleton :: Proxy a -> SizedSingletonElement a -> a

-- | Build a 'Sized' value from a singleton value
singleton :: SizedSingleton a => Proxy a -> SizedSingletonElement a -> Sized (Exactly 1) a
singleton p = trustedSized . sizedSingleton p
{-# INLINE singleton #-}

-- | Build 'SizedSingleton' for 'Applicative' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance SizedSingleton [a] via (MkSizedSingletonApplicative [a])
newtype MkSizedSingletonApplicative a
  = MkSizedSingletonApplicative a

instance Applicative f => SizedSingleton (f a) where
  type SizedSingletonElement (f a) = a
  sizedSingleton _ = pure

-- * From container

-- | Used to attempt conversion from possibly any size to 'Sized'.
class SizedFromContainer a where
  calculateSize :: a -> Int
  isAtLeast :: Int -> a -> Bool
  isAtLeast n = (>= n) . calculateSize
  isAtMost :: Int -> a -> Bool
  isAtMost n = (<= n) . calculateSize
  isExactly :: Int -> a -> Bool
  isExactly n = (== n) . calculateSize
  isBetween :: Int -> Int -> a -> Bool
  isBetween n m = inRange (n, m) . calculateSize

-- | Convert a container from possibly any size to 'Sized'.
class Size s where
  sized :: SizedFromContainer a => a -> Maybe (Sized s a)

instance Size Unknown where
  sized = Just . trustedSized

instance KnownNat n => Size (Exactly n) where
  sized = mkSized $ isExactly $ fromInteger $ natVal $ Proxy @n

instance KnownNat n => Size (AtLeast n) where
  sized = mkSized $ isAtLeast $ fromInteger $ natVal $ Proxy @n

instance KnownNat n => Size (AtMost n) where
  sized = mkSized $ isAtMost $ fromInteger $ natVal $ Proxy @n

instance (KnownNat n, KnownNat m, n <= m) => Size (Between n m) where
  sized = mkSized $ isBetween (fromInteger $ natVal $ Proxy @n) (fromInteger $ natVal $ Proxy @m)

mkSized :: (a -> Bool) -> a -> Maybe (Sized s a)
mkSized p x =
  if p x
    then Just $ trustedSized x
    else Nothing

-- | Build 'MkSizedFromContainerFoldable' for 'Foldable' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance SizedFromContainer [a] via (MkSizedFromContainerFoldable [a])
newtype MkSizedFromContainerFoldable a
  = MkSizedFromContainerFoldable a

instance Foldable f => SizedFromContainer (f a) where
  calculateSize = length
  isExactly n = (== [n]) . drop n . index
  isAtLeast n = not . null . drop n . index
  isAtMost n = null . drop n . toList
  isBetween n m x =
    let indexed = index x
      in  elem n indexed && notElem (succ m) indexed

index :: Foldable f => f a -> [Int]
index = (0:) . zipWith const [1..] . toList

-- | Give a more precise sizing
precise :: (SizedFromContainer a, Size s) => Sized s' a -> Maybe (Sized s a)
precise = sized . getSized
{-# INLINE precise #-}

type family IsMoreGeneral general restrictive :: Constraint where
  IsMoreGeneral (AtLeast n) (AtLeast m) = (n <= m)
  IsMoreGeneral (AtMost n) (AtMost m) = (m <= n)
  IsMoreGeneral (AtLeast n) (Exactly m) = (n <= m)
  IsMoreGeneral (AtMost n) (Exactly m) = (m <= n)
  IsMoreGeneral (AtLeast n) (Between m o) = (n <= m)
  IsMoreGeneral (AtMost n) (Between m o) = (o <= n)
  IsMoreGeneral (Between n m) (Between o p) = (n <= o, p <= m)
  IsMoreGeneral (Between n m) (Exactly o) = (n <= o, o <= m)
  IsMoreGeneral Unknown a = ()

-- | Give a more general sizing
approximate :: IsMoreGeneral s s' => Sized s' a -> Sized s a
approximate = trustedSized . getSized
{-# INLINE approximate #-}

-- | Concatenative operation of 'Sized'
withSizedAppend :: (a -> b -> c) -> Sized s a -> Sized s' b -> Sized (s <+> s') c
withSizedAppend f x y = trustedSized $ f (getSized x) (getSized y)

type family (<+>) a b where
  (<+>) Unknown b = Unknown
  (<+>) a Unknown = Unknown
  (<+>) (AtMost n) (AtMost m) = AtMost (n + m)
  (<+>) (AtLeast n) (AtLeast m) = AtLeast (n + m)
  (<+>) (AtLeast n) (AtMost m) = Between n (n + m)
  (<+>) (AtMost n) (AtLeast m) = Between m (n + m)
  (<+>) (AtLeast n) (Between m o) = Between (n + m) (n + o)
  (<+>) (AtMost n) (Between m o) = Between (n + m) (n + o)
  (<+>) (Between m o) (AtLeast n) = Between (n + m) (n + o)
  (<+>) (Between m o) (AtMost n) = Between (n + m) (n + o)
  (<+>) (Between n m) (Between o p) = Between (n + m) (o + p)

-- | Semigroup append
(<<>>) :: Semigroup a => Sized s a -> Sized s' a -> Sized (s <+> s') a
(<<>>) = withSizedAppend (<>)
{-# INLINE (<<>>) #-}

infixr 6 <<>>
