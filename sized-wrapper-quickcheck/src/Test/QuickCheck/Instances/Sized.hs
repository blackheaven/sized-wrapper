{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module        : Test.QuickCheck.Instances.Sized
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- QuickCheck's 'Arbitrary' instance for 'Sized'.
module Test.QuickCheck.Instances.Sized
  ( Arbitrary (..),
    ArbitrarySized (..),
    ArbitraryBuiltSized (..),
  )
where

import Control.Monad
import Data.Maybe (maybeToList)
import Data.Proxy
import Data.Sized
import GHC.TypeLits
import Test.QuickCheck hiding (sized)

instance
  ( Size s,
    SizedFromContainer a,
    Arbitrary a,
    ArbitraryBuiltSized a,
    ArbitrarySized s
  ) =>
  Arbitrary (Sized s a)
  where
  arbitrary = arbitrarySized (Proxy @s) >>= fmap trustedSized . buildSized

  shrink xs =
    [ xs''
      | xs' <- shrink $ getSized xs,
        xs'' <- maybeToList $ sized xs'
    ]

class ArbitrarySized a where
  arbitrarySized :: Proxy a -> Gen Int

instance ArbitrarySized Unknown where
  arbitrarySized _ = chooseInt (0, maxBound)

instance KnownNat n => ArbitrarySized (AtLeast n) where
  arbitrarySized _ = chooseInt (fromInteger $ natVal $ Proxy @n, maxBound)

instance KnownNat n => ArbitrarySized (AtMost n) where
  arbitrarySized _ = chooseInt (0, fromInteger $ natVal $ Proxy @n)

instance KnownNat n => ArbitrarySized (Exactly n) where
  arbitrarySized _ = return $ fromInteger $ natVal $ Proxy @n

instance (KnownNat n, KnownNat m) => ArbitrarySized (Between n m) where
  arbitrarySized _ = chooseInt (fromInteger $ natVal $ Proxy @n, fromInteger $ natVal $ Proxy @m)

class ArbitraryBuiltSized a where
  buildSized :: Int -> Gen a

instance Arbitrary a => ArbitraryBuiltSized [a] where
  buildSized n = replicateM n arbitrary
