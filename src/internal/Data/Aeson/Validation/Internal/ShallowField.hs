module Data.Aeson.Validation.Internal.ShallowField
  ( ShallowField(..)
  , fieldSchemaL
  ) where

import Data.Aeson.Validation.Internal.Prelude
import Data.Aeson.Validation.Internal.Types


fieldSchemaL :: Lens' ShallowField Schema
fieldSchemaL f (ShallowField a b c) =
  (\c' -> ShallowField a b c') <$> f c
