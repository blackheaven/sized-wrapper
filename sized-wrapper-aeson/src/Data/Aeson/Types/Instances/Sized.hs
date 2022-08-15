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
-- aeson instances for 'Sized'
module Data.Aeson.Types.Instances.Sized
  ( FromJSON (..),
    ToJSON (..),
    FromJSONKey (..),
    ToJSONKey (..),
  )
where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Data.Sized

instance
  ( FromJSON a,
    Semigroup a,
    Size s,
    SizedSingleton a,
    SizedFromContainer a,
    FromJSON (SizedSingletonElement a)
  ) =>
  FromJSON (Sized s a)
  where
  parseJSON x = do
    raw <- parseJSON x
    case sized raw of
      Just y -> pure y
      Nothing -> fail "parsing Sized failed, unexpected empty container"

instance
  ( FromJSON a,
    Size s,
    SizedSingleton a,
    SizedFromContainer a,
    FromJSON (SizedSingletonElement a),
    FromJSONKey (SizedSingletonElement a),
    FromJSONKey a,
    Semigroup a
  ) =>
  FromJSONKey (Sized s a)
  where
  fromJSONKey =
    case fromJSONKey @a of
      FromJSONKeyCoerce -> FromJSONKeyTextParser (run . coerce)
      FromJSONKeyText f -> FromJSONKeyTextParser (run . f)
      FromJSONKeyTextParser f -> FromJSONKeyTextParser (f >=> run)
      FromJSONKeyValue f -> FromJSONKeyValue (f >=> run)
    where
      run :: a -> Parser (Sized s a)
      run x =
        case sized x of
          Just y -> pure y
          Nothing -> fail "parsing Sized failed, unexpected empty container"

instance (ToJSON a) => ToJSON (Sized s a) where
  toJSON = toJSON . getSized
  toEncoding = toEncoding . getSized
  toJSONList = toJSONList . map getSized
  toEncodingList = toEncodingList . map getSized

instance (ToJSONKey a) => ToJSONKey (Sized s a) where
  toJSONKey = contramapToJSONKeyFunction getSized toJSONKey
  toJSONKeyList = contramapToJSONKeyFunction (map getSized) toJSONKeyList
