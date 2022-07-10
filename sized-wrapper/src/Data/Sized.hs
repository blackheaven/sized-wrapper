{-# LANGUAGE FlexibleInstances #-}

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

    -- * Singleton constructor
    SizedSingleton (..),
    singleton,
    MkSizedSingletonApplicative (..),

    -- * From container
    SizedFromContainer (..),
    nonEmpty,
    MkSizedFromContainerFoldable (..),

    -- * Operations
    (<|),
    (|>),
    overSized,
    overSized2,
    overSized3,
    overSized4,
    overSized5,
    fmapSized,
    withSized,
  )
where

import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy

-- | Sized proofed value.
newtype Sized a = Sized
  { -- | Extract the Sized proven value
    getSized :: a
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup a => Semigroup (Sized a) where
  Sized x <> Sized y = Sized $ x <> y

-- * Operations

-- | Append empty container
(<|) :: Semigroup a => Sized a -> a -> Sized a
Sized ne <| n = Sized $ ne <> n
{-# INLINE (<|) #-}

infixr 6 <|

-- | Prepend empty container
(|>) :: Semigroup a => a -> Sized a -> Sized a
n |> Sized ne = Sized $ n <> ne
{-# INLINE (|>) #-}

infixr 6 |>

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized :: (a -> b) -> Sized a -> Sized b
overSized f = trustedSized . f . getSized
{-# INLINE overSized #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized2 :: (a -> b -> c) -> Sized a -> Sized b -> Sized c
overSized2 f a = trustedSized . f (getSized a) . getSized
{-# INLINE overSized2 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized3 :: (a -> b -> c -> d) -> Sized a -> Sized b -> Sized c -> Sized d
overSized3 f a b = trustedSized . f (getSized a) (getSized b) . getSized
{-# INLINE overSized3 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized4 :: (a -> b -> c -> d -> e) -> Sized a -> Sized b -> Sized c -> Sized d -> Sized e
overSized4 f a b c = trustedSized . f (getSized a) (getSized b) (getSized c) . getSized
{-# INLINE overSized4 #-}

-- | Wrap and unwrap 'Sized' (unsafe, be sure 'f' is size-conservative)
overSized5 :: (a -> b -> c -> d -> e -> f) -> Sized a -> Sized b -> Sized c -> Sized d -> Sized e -> Sized f
overSized5 f a b c d = trustedSized . f (getSized a) (getSized b) (getSized c) (getSized d) . getSized
{-# INLINE overSized5 #-}

-- | 'fmap' over a 'Sized' container
fmapSized :: Functor f => (a -> b) -> Sized (f a) -> Sized (f b)
fmapSized f = overSized (fmap f)
{-# INLINE fmapSized #-}

-- | Apply an unsafe function over empty, which is safe over 'Sized'
withSized :: (a -> Maybe b) -> Sized a -> b
withSized f = fromJust . f . getSized
{-# INLINE withSized #-}

-- | Trusted value
trustedSized :: a -> Sized a
trustedSized = Sized
{-# INLINE trustedSized #-}

-- | Singleton constructible value
class SizedSingleton a where
  type SizedSingletonElement a :: Type
  nonEmptySingleton :: Proxy a -> SizedSingletonElement a -> a

-- | Build a 'Sized' value from a singleton value
singleton :: SizedSingleton a => Proxy a -> SizedSingletonElement a -> Sized a
singleton p = trustedSized . nonEmptySingleton p
{-# INLINE singleton #-}

-- | Build 'SizedSingleton' for 'Applicative' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance SizedSingleton [a] via (MkSizedSingletonApplicative [a])
newtype MkSizedSingletonApplicative a
  = MkSizedSingletonApplicative a

instance Applicative f => SizedSingleton (f a) where
  type SizedSingletonElement (f a) = a
  nonEmptySingleton _ = pure

-- * From container

-- | Used to attempt conversion from possibly empty to 'Sized'.
class SizedFromContainer a where
  isSized :: a -> Bool

-- | Attempt 'Sized' proof
nonEmpty :: SizedFromContainer a => a -> Maybe (Sized a)
nonEmpty x =
  if isSized x
    then Just $ trustedSized x
    else Nothing

-- | Build 'MkSizedFromContainerFoldable' for 'Foldable' defined types
--   to be used with 'DerivingVia':
--
--   > deriving instance SizedFromContainer [a] via (MkSizedFromContainerFoldable [a])
newtype MkSizedFromContainerFoldable a
  = MkSizedFromContainerFoldable a

instance Foldable f => SizedFromContainer (f a) where
  isSized = not . null
