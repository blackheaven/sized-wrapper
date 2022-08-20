{-# LANGUAGE ConstraintKinds #-}
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
    trustedChangeOverSized,
    trustedChangeOverSized2,
    trustedChangeOverSized3,
    trustedChangeOverSized4,
    trustedChangeOverSized5,
    fmapSized,
    withSized,
    precise,
    approximate,
    (<<>>),
    withSizedAppend,
    withSizedRetract,
    withSizedSubtract,
    withSizedProduct,

    -- * Type-level
    IsMoreGeneral,
    IsNotEmpty,
    type (<+>),
    RestrictAtMost,
    type (<->),
    type (<*>),
    Includes,
  )
where

import Data.Foldable (toList)
import Data.Ix (inRange)
import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy
import GHC.TypeLits as TypeLits

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

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is respects output size)
trustedChangeOverSized :: (a -> b) -> Sized s0 a -> Sized s1 b
trustedChangeOverSized f = trustedSized . f . getSized
{-# INLINE trustedChangeOverSized #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is respects output size)
trustedChangeOverSized2 :: (a -> b -> c) -> Sized s0 a -> Sized s1 b -> Sized s2 c
trustedChangeOverSized2 f a = trustedSized . f (getSized a) . getSized
{-# INLINE trustedChangeOverSized2 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is respects output size)
trustedChangeOverSized3 :: (a -> b -> c -> d) -> Sized s0 a -> Sized s1 b -> Sized s2 c -> Sized s3 d
trustedChangeOverSized3 f a b = trustedSized . f (getSized a) (getSized b) . getSized
{-# INLINE trustedChangeOverSized3 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is respects output size)
trustedChangeOverSized4 :: (a -> b -> c -> d -> e) -> Sized s0 a -> Sized s1 b -> Sized s2 c -> Sized s3 d -> Sized s4 e
trustedChangeOverSized4 f a b c = trustedSized . f (getSized a) (getSized b) (getSized c) . getSized
{-# INLINE trustedChangeOverSized4 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is respects output size)
trustedChangeOverSized5 :: (a -> b -> c -> d -> e -> f) -> Sized s0 a -> Sized s1 b -> Sized s2 c -> Sized s3 d -> Sized s4 e -> Sized s5 f
trustedChangeOverSized5 f a b c d = trustedSized . f (getSized a) (getSized b) (getSized c) (getSized d) . getSized
{-# INLINE trustedChangeOverSized5 #-}

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
     in elem n indexed && notElem (succ m) indexed

index :: Foldable f => f a -> [Int]
index = (0 :) . zipWith const [1 ..] . toList

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

type IsNotEmpty s = IsMoreGeneral s (Exactly 1)

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
  (<+>) (AtMost n) (Exactly m) = AtMost (n + m)
  (<+>) (Exactly n) (Exactly m) = Exactly (n + m)
  (<+>) (Exactly n) (AtMost m) = AtMost (n + m)
  (<+>) (Exactly n) (AtLeast m) = AtLeast (n + m)
  (<+>) (AtLeast n) (Exactly m) = AtLeast (n + m)
  (<+>) (AtLeast n) (AtLeast m) = AtLeast (n + m)
  (<+>) (AtLeast n) (AtMost m) = Between n (n + m)
  (<+>) (AtMost n) (AtLeast m) = Between m (n + m)
  (<+>) (AtLeast n) (Between m o) = Between (n + m) (n + o)
  (<+>) (AtMost n) (Between m o) = Between m (n + o)
  (<+>) (Between n m) (Exactly o) = Between (n + o) (m + o)
  (<+>) (Exactly o) (Between n m) = Between (n + o) (m + o)
  (<+>) (Between m o) (AtLeast n) = Between (n + m) (n + o)
  (<+>) (Between m o) (AtMost n) = Between m (n + o)
  (<+>) (Between n m) (Between o p) = Between (n + o) (m + p)

-- | Semigroup append
(<<>>) :: Semigroup a => Sized s a -> Sized s' a -> Sized (s <+> s') a
(<<>>) = withSizedAppend (<>)
{-# INLINE (<<>>) #-}

infixr 6 <<>>

type family RestrictAtMost a where
  RestrictAtMost Unknown = Unknown
  RestrictAtMost (AtMost n) = AtMost n
  RestrictAtMost (AtLeast n) = AtLeast 0
  RestrictAtMost (Between n m) = AtMost m
  RestrictAtMost (Exactly n) = AtMost n

withSizedRetract :: (a -> b) -> Sized s a -> Sized (RestrictAtMost s) b
withSizedRetract f = trustedSized . f . getSized

type family (<->) a b where
  (<->) Unknown b = Unknown
  (<->) a Unknown = Unknown
  (<->) (AtMost n) (AtMost m) = AtMost (n - m)
  (<->) (Exactly n) (AtMost m) = AtMost (n - m)
  (<->) (AtMost n) (Exactly m) = AtMost (n - m)
  (<->) (Exactly n) (Exactly m) = Exactly (n - m)
  (<->) (Exactly n) (AtLeast m) = AtLeast (n - m)
  (<->) (AtLeast n) (Exactly m) = AtLeast (n - m)
  (<->) (AtLeast n) (AtLeast m) = AtLeast (n - m)
  (<->) (AtLeast n) (AtMost m) = AtLeast (n - m)
  (<->) (AtMost n) (AtLeast m) = AtLeast 0
  (<->) (AtLeast n) (Between m o) = AtLeast (m - n)
  (<->) (AtMost n) (Between m o) = Between (m - n) o
  (<->) (Between n m) (Exactly o) = Between (n - o) (m - o)
  (<->) (Exactly o) (Between n m) = Between (o - n) (o - m)
  (<->) (Between m o) (AtLeast n) = AtLeast 0
  (<->) (Between m o) (AtMost n) = Between m (o - n)
  (<->) (Between n m) (Between o p) = Between (n - o) (m - p)

withSizedSubtract :: (a -> b -> c) -> Sized s a -> Sized s' b -> Sized (s <-> s') c
withSizedSubtract f a b = trustedSized $ f (getSized a) (getSized b)

type family (<*>) a b where
  (<*>) Unknown b = Unknown
  (<*>) a Unknown = Unknown
  (<*>) (AtMost n) (AtMost m) = AtMost (n TypeLits.* m)
  (<*>) (Exactly n) (AtMost m) = AtMost (n TypeLits.* m)
  (<*>) (AtMost n) (Exactly m) = AtMost (n TypeLits.* m)
  (<*>) (Exactly n) (Exactly m) = Exactly (n TypeLits.* m)
  (<*>) (AtLeast n) (Exactly m) = AtLeast (n TypeLits.* m)
  (<*>) (Exactly n) (AtLeast m) = AtLeast (n TypeLits.* m)
  (<*>) (AtLeast n) (AtLeast m) = AtLeast (n TypeLits.* m)
  (<*>) (AtLeast n) (AtMost m) = AtLeast 0
  (<*>) (AtMost n) (AtLeast m) = AtLeast 0
  (<*>) (AtLeast n) (Between m o) = AtLeast (m TypeLits.* n)
  (<*>) (AtMost n) (Between m o) = AtLeast 0
  (<*>) (Between n m) (Exactly o) = Between (n TypeLits.* o) (m TypeLits.* o)
  (<*>) (Exactly o) (Between n m) = Between (n TypeLits.* o) (m TypeLits.* o)
  (<*>) (Between m o) (AtLeast n) = AtLeast (m TypeLits.* n)
  (<*>) (Between m o) (AtMost n) = AtLeast 0
  (<*>) (Between n m) (Between o p) = Between (n TypeLits.* o) (m TypeLits.* p)

withSizedProduct :: (a -> b -> c) -> Sized s a -> Sized s' b -> Sized (s <*> s') c
withSizedProduct f a b = trustedSized $ f (getSized a) (getSized b)

type family Includes (sized :: Type) (size :: Nat) :: Constraint where
  Includes (AtLeast n) m = (n <= m)
  Includes (AtMost n) m = (m <= n)
  Includes (Between n m) o = (n <= o, o <= m)
  Includes Unknown a = ()
