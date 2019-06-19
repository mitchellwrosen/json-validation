-- | Mutually recursive types.

module Data.Aeson.Validation.Internal.Types where

import Data.Aeson.Validation.Internal.Prelude

import Data.List.NonEmpty ((<|))

import qualified GHC.Exts as GHC


-- $setup
-- >>> import Data.Aeson.Validation.Internal.Schema

data Demand
  = Opt
  | Req
  deriving Eq

instance Hashable Demand where
  hash Opt = 0
  hash Req = 1

  -- Stolen from @hashable@
  hashWithSalt s x = s * 16777619 `xor` hash x

-- | An opaque object 'Field'.
--
-- Create a 'Field' with '.:' or '.:?', and bundle it into a 'Schema' using
-- 'object' or 'object''
data Field
  = Field !Demand !(NonEmpty Text) !Schema

-- | An opaque JSON 'Schema'.
data Schema
  = SBool
  | STrue
  | SFalse
  | SNumber
  | SInteger
  | STheNumber !Scientific
  | STheInteger !Integer
  | SSomeNumber !Text {- error msg -} (Scientific -> Bool) {- predicate -}
  | SString
  | STheString !Text
  | SSomeString !Text {- error msg -} (Text -> Bool) {- predicate -}
  | SDateTime
  | SObject !Strict ![ShallowField]
  | SArray !Unique !Int {- min len -} !Int {- max len -} !Schema
  | STuple ![Schema]
  | SAnything
  | SNullable !Schema
  | SAlts !(NonEmpty Schema)
  | SNegate !Schema

-- | The 'Num' instance only defines two functions; all other 'Num' functions
-- call 'error'.
--
--     (1) 'fromInteger' is provided for integer-literal syntax.
--
--         @
--         'fromInteger' = 'theInteger'
--         @
--
--         Examples:
--
--         >>> validate 1 (Number 1)
--         []
--
--     (2) @'negate' s@ succeeds whenever @s@ fails.
--
--         'negate' is its own inverse:
--
--         @
--         'negate' . 'negate' = 'id'
--         @
--
--         Examples:
--
--         >>> validate (negate bool) (String "foo")
--         []
--
--         >>> validate (negate bool) (Bool True)
--         ["expected anything but a bool but found true"]
instance Num Schema where
  (+)    = error "Data.Aeson.Validation: (+) not implemented for Schema"
  (-)    = error "Data.Aeson.Validation: (-) not implemented for Schema"
  (*)    = error "Data.Aeson.Validation: (*) not implemented for Schema"
  abs    = error "Data.Aeson.Validation: abs not implemented for Schema"
  signum = error "Data.Aeson.Validation: signum not implemented for Schema"

  fromInteger = STheInteger
  negate = SNegate

-- | The 'Fractional' instance only defines one function; all other 'Fractional'
-- functions call 'error'.
--
--     (1) 'fromRational' is provided for floating point-literal syntax.
--
--         @
--         'fromRational' = 'theNumber' . 'fromRational'
--         @
--
--         Examples:
--
--         >>> validate 1.5 (Number 1.5)
--         []
--
--         >>> validate 2.5 (Number 2.500000001)
--         []
instance Fractional Schema where
  (/)   = error "Data.Aeson.Validation: (/) not implemented for Schema"
  recip = error "Data.Aeson.Validation: recip not implemented for Schema"

  fromRational = STheNumber . fromRational

-- | The '<>' operator is used to create a /sum/ 'Schema' that, when applied to
-- a 'Value', first tries the left 'Schema', then falls back on the right one if
-- the left one fails.
--
-- For 'validate', if any 'Schema's emits no violations, then no violations are
-- emitted. Otherwise, all violations are emitted.
--
-- Examples:
--
-- >>> validate (bool <> string) (Bool True)
-- []
--
-- >>> validate (bool <> string) (String "foo")
-- []
--
-- >>> validate (bool <> string) (Number 1)
-- ["expected a bool but found a number","expected a string but found a number"]
instance Semigroup Schema where
  SAlts xs      <> SAlts ys = SAlts (xs <> ys)
  SAlts (x:|xs) <> y        = SAlts (x :| xs ++ [y]) -- Won't happen naturally (infixr)
  x             <> SAlts ys = SAlts (x <| ys)
  x             <> y        = SAlts (x :| [y])

-- | 'GHC.fromString' is provided for string-literal syntax.
--
-- @
-- 'GHC.fromString' = 'theString' . 'Data.Text.pack'
-- @
--
-- Examples:
--
-- >>> validate "foo" (String "foo")
-- []
instance GHC.IsString Schema where
  fromString = STheString . GHC.fromString

data ShallowField
  = ShallowField
  { fieldDemand :: !Demand
  , fieldKey :: !Text
  , fieldSchema :: !Schema
  }

-- Are extra properties of an object allowed?
data Strict
  = Strict
  | NotStrict

-- Are duplicate elements in an array allowed?
data Unique
  = Unique
  | NotUnique
  deriving Eq
