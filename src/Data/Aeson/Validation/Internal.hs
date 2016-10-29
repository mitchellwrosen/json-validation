module Data.Aeson.Validation.Internal where

import Data.Bits                     (xor)
import Data.Generics.Str
import Data.Generics.Uniplate.Direct
import Data.Hashable                 (Hashable(..))
import Data.List.NonEmpty            (NonEmpty(..), (<|))
import Data.Scientific
import Data.Semigroup
import Data.Text                     (Text)

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

instance Uniplate Schema where
  uniplate = \case
    SBool               -> plate SBool
    STrue               -> plate STrue
    SFalse              -> plate SFalse
    SNumber             -> plate SNumber
    SInteger            -> plate SInteger
    STheNumber  a       -> plate STheNumber  |-  a
    STheInteger a       -> plate STheInteger |-  a
    SSomeNumber a b     -> plate SSomeNumber |-  a |-  b
    SString             -> plate SString
    STheString  a       -> plate STheString  |-  a
    SSomeString a b     -> plate SSomeString |-  a |-  b
    SObject     a b     -> plate SObject     |-  a ||+ b
    SArray      a b c d -> plate SArray      |-  a |-  b |- c |* d
    STuple      a       -> plate STuple      ||* a
    SAnything           -> plate SAnything
    SNullable   a       -> plate SNullable   |*  a
    SAlts       a       -> (nonEmptyStr a, SAlts . strNonEmpty)
    SNegate     a       -> plate SNegate     |*  a
   where
    nonEmptyStr :: NonEmpty a -> Str a
    nonEmptyStr = listStr . NonEmpty.toList

    strNonEmpty :: Str a -> NonEmpty a
    strNonEmpty = NonEmpty.fromList . strList

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
  = Field !Demand !Path !Schema

data ShallowField = ShallowField
  { fieldDemand :: !Demand
  , fieldKey    :: !Text
  , fieldSchema :: !Schema
  }

instance Biplate ShallowField Schema where
  biplate (ShallowField a b c) = plate ShallowField |- a |- b |* c

-- | An arbitrarily deep non-empty 'Path' into an 'Object', created with either
-- string-literal or list-literal syntax.
--
-- Beware: the 'GHC.IsList' instance is partial; @[]@ is not allowed and will
-- call 'error'.
--
-- ==== __Examples__
--
-- >>> "foo" :: Path
-- ["foo"]
--
-- >>> ["foo", "bar"] :: Path
-- ["foo","bar"]
--
-- >>> [] :: Path
-- *** Exception: Data.Aeson.Validation.Path.fromList: empty list
data Path
  = Link !Text !Path
  | Leaf !Text
  deriving (Eq, Ord)

instance Show Path where
  show = show . GHC.toList

-- | A singleton 'Path'.
instance GHC.IsString Path where
  fromString = Leaf . GHC.fromString

-- | 'Path's created with @[]@ syntax must be non-empty.
instance GHC.IsList Path where
  type Item Path = Text

  fromList :: [GHC.Item Path] -> Path
  fromList [] =
    errorWithoutStackTrace "Data.Aeson.Validation.Path.fromList: empty list"
  fromList xs0 = go xs0
   where
    go :: [Text] -> Path
    go []     = error "impossible"
    go [x]    = Leaf x
    go (x:xs) = Link x (go xs)

  toList :: Path -> [GHC.Item Path]
  toList (Leaf x)    = [x]
  toList (Link x xs) = x : GHC.toList xs

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
