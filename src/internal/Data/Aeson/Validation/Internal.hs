{-# language LambdaCase #-}

module Data.Aeson.Validation.Internal where

-- import Control.Applicative ((<$>))
-- import Data.Bits (xor)
-- import Data.Hashable (Hashable(..))
-- import Data.List.NonEmpty (NonEmpty(..), (<|))
-- import Data.Scientific
-- import Data.Semigroup
-- import Data.Text (Text)
-- import Lens.Micro

-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified GHC.Exts as GHC

-- $setup
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Validation

-- data ShallowField = ShallowField
--   { fieldDemand :: !Demand
--   , fieldKey    :: !Text
--   , fieldSchema :: !Schema
--   }

-- fieldSchemaL :: Lens' ShallowField Schema
-- fieldSchemaL f (ShallowField a b c) = (\c' -> ShallowField a b c') <$> f c
