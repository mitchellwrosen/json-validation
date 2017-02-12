module Data.Aeson.Validation.Internal where

import Data.Bits          (xor)
import Data.Hashable      (Hashable(..))
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Scientific
import Data.Semigroup
import Data.Text          (Text)
import Lens.Micro
import Prelude.Compat

import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Exts           as GHC

-- $setup
-- >>> import Data.Aeson (Value(..))
-- >>> import Data.Aeson.Validation

-- | An opaque JSON 'Schema'.
data Schema
  = SBool
  | STrue
  | SFalse
  | SNumber
  | SInteger
  | STheNumber !Scientific
  | STheInteger !Integer
  | SSomeNumber (Scientific -> Bool) !(Maybe Text) {- error msg -}
  | SString
  | STheString !Text
  | SSomeString (Text -> Bool) !(Maybe Text) {- error msg -}
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
--         >>> schema 1 (Number 1)
--         True
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
--         >>> schema (negate bool) (Bool True)
--         False
--
--         >>> schema (negate bool) (String "foo")
--         True
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
--         >>> schema 1.5 (Number 1.5)
--         True
--
--         >>> schema 2.5 (Number 2.500000001)
--         True
instance Fractional Schema where
  (/)   = error "Data.Aeson.Validation: (/) not implemented for Schema"
  recip = error "Data.Aeson.Validation: recip not implemented for Schema"

  fromRational = STheNumber . fromRational

-- | The '<>' operator is used to create a /sum/ 'Schema' that, when applied to
-- a 'Value', first tries the left 'Schema', then falls back on the right one if
-- the left one fails.
--
-- @
-- 'schema' (s1 '<>' s2) val = 'schema' s1 val '||' 'schema' s2 val
-- @
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
-- >>> schema "foo" (String "foo")
-- True
instance GHC.IsString Schema where
  fromString = STheString . GHC.fromString

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

data ShallowField = ShallowField
  { fieldDemand :: !Demand
  , fieldKey    :: !Text
  , fieldSchema :: !Schema
  }

fieldSchemaL :: Lens' ShallowField Schema
fieldSchemaL f (ShallowField a b c) = (\c' -> ShallowField a b c') <$> f c

-- Are extra properties of an object allowed?
data Strict
  = Strict
  | NotStrict

-- Are duplicate elements in an array allowed?
data Unique
  = Unique
  | NotUnique
  deriving Eq

normalize :: Schema -> Schema
normalize = transform
  (\case
    SAlts ss ->
      SAlts (NonEmpty.fromList (NonEmpty.toList ss >>= unAlt))
    s -> s)
 where
  -- Rip off top-level SAlts constructor
  unAlt :: Schema -> [Schema]
  unAlt (SAlts ss) = NonEmpty.toList ss
  unAlt s = [s]

-- Uniplate stuff, not enough to be worth the dependency

universe :: Schema -> [Schema]
universe = \case
  SBool               -> [SBool]
  STrue               -> [STrue]
  SFalse              -> [SFalse]
  SNumber             -> [SNumber]
  SInteger            -> [SInteger]
  STheNumber  a       -> [STheNumber a]
  STheInteger a       -> [STheInteger a]
  SSomeNumber a b     -> [SSomeNumber a b]
  SString             -> [SString]
  STheString  a       -> [STheString a]
  SSomeString a b     -> [SSomeString a b]
  SDateTime           -> [SDateTime]
  SObject     a b     -> SObject a b : concatMap universe (map fieldSchema b)
  SArray      a b c d -> SArray a b c d : universe d
  STuple      a       -> STuple a : concatMap universe a
  SAnything           -> [SAnything]
  SNullable   a       -> SNullable a : universe a
  SAlts       a       -> SAlts a : concatMap universe (NonEmpty.toList a)
  SNegate     a       -> SNegate a : universe a

-- Transform all 'Schema', bottom up.
transform :: (Schema -> Schema) -> Schema -> Schema
transform f = f . transform' (transform f)

-- Transform direct children.
transform' :: (Schema -> Schema) -> Schema -> Schema
transform' f = \case
  SBool               -> SBool
  STrue               -> STrue
  SFalse              -> SFalse
  SNumber             -> SNumber
  SInteger            -> SInteger
  STheNumber  a       -> STheNumber a
  STheInteger a       -> STheInteger a
  SSomeNumber a b     -> SSomeNumber a b
  SString             -> SString
  STheString  a       -> STheString a
  SSomeString a b     -> SSomeString a b
  SDateTime           -> SDateTime
  SObject     a b     -> SObject a (b & each.fieldSchemaL %~ f)
  SArray      a b c d -> SArray a b c (f d)
  STuple      a       -> STuple (map f a)
  SAnything           -> SAnything
  SNullable   a       -> SNullable (f a)
  SAlts       a       -> SAlts (fmap f a)
  SNegate     a       -> SNegate (f a)
