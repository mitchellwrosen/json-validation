{-# language DeriveGeneric #-}

module Data.Aeson.Validation.Internal.Pair
  ( Pair(..)
  ) where

import Data.Aeson.Validation.Internal.Prelude


data Pair a b
  = Pair !a !b
  deriving (Eq, Generic)

instance (Hashable a, Hashable b) => Hashable (Pair a b)
