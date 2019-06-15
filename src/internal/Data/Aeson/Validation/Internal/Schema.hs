{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Data.Aeson.Validation.Internal.Schema where

import Data.Aeson.Validation.Internal.Field (flatten)
import Data.Aeson.Validation.Internal.ShallowField (fieldSchemaL)
import Data.Aeson.Validation.Internal.Prelude
import Data.Aeson.Validation.Internal.Types

import Text.Regex.PCRE.Light (Regex)

import qualified Text.Regex.PCRE.Light as Regex
import qualified Data.List.NonEmpty as NonEmpty


--------------------------------------------------------------------------------
-- Schema
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Schema smart constructors
--------------------------------------------------------------------------------

-- | Any 'Data.Aeson.Types.Bool'.
--
-- ==== __Examples__
--
-- >>> schema bool (Bool True)
-- True
--
-- >>> schema bool (Bool False)
-- True
bool :: Schema
bool =
  SBool

-- | 'Data.Aeson.Types.Bool' 'True'.
--
-- ==== __Examples__
--
-- >>> schema true (Bool True)
-- True
--
-- >>> schema true (Bool False)
-- False
true :: Schema
true =
  STrue

-- | 'Data.Aeson.Types.Bool' 'False'.
--
-- ==== __Examples__
--
-- >>> schema false (Bool True)
-- False
--
-- >>> schema false (Bool False)
-- True
false :: Schema
false =
  SFalse

-- | Any 'Number'.
--
-- ==== __Examples__
--
-- >>> schema number (Number 1.0)
-- True
number :: Schema
number =
  SNumber

-- | An /approximate/ 'Data.Aeson.Types.Number', with a relative tolerance of
-- @1e-9@ (the smaller number must be within @0.0000001%@ of the larger number).
--
-- You may use a floating point literal instead.
--
-- Here is how you'd implement 'theNumber' using 'someNumber', in case you want
-- to use a different tolerance:
--
-- @
-- 'theNumber' :: 'Scientific' -> 'Schema'
-- 'theNumber' n = 'someNumber' (isClose n)
--   where
--     isClose :: 'Scientific' -> 'Scientific' -> 'Bool'
--     isClose a b = 'abs' (a-b) '<=' 1e-9 * 'max' ('abs' a) ('abs' b)
-- @
--
-- ==== __Examples__
--
-- >>> schema (theNumber 1.5) (Number 1.5)
-- True
--
-- >>> schema 2.5 (Number 2.500000001)
-- True
theNumber :: Scientific -> Schema
theNumber =
  STheNumber

-- | Any integer 'Number'.
--
-- ==== __Examples__
--
-- >>> schema integer (Number 1.0)
-- True
--
-- >>> schema integer (Number 1.5)
-- False
integer :: Schema
integer =
  SInteger

-- | An exact integer 'Data.Aeson.Types.Number'.
--
-- You may use an integer literal instead.
--
-- ==== __Examples__
--
-- >>> schema (theInteger 1) (Number 1)
-- True
--
-- >>> schema 1 (Number 2)
-- False
theInteger :: Integer -> Schema
theInteger =
  STheInteger

-- | Any 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> schema string (String "foo")
-- True
string :: Schema
string =
  SString

-- | An exact 'Data.Aeson.Types.String'.
--
-- You may use a string literal instead (requires @-XOverloadedStrings@).
--
-- ==== __Examples__
--
-- >>> schema (theString "foo") (String "foo")
-- True
--
-- >>> schema "foo" (String "bar")
-- False
theString :: Text -> Schema
theString =
  STheString

-- | A 'Data.Aeson.Types.String' that matches a regular expression.
--
-- ==== __Examples__
--
-- >>> schema (regex "a+b") (String "xaaabx")
-- True
--
-- >>> schema (regex "c{2}") (String "cd")
-- False
regex :: Text -> Schema
regex r =
  SSomeString p (Just ("matches " <> r))
  where
    p :: Text -> Bool
    p s =
      has (_Just . _head) (Regex.match r' (encodeUtf8 s) [])

    r' :: Regex
    r' =
      Regex.compile (encodeUtf8 r) [Regex.utf8]

-- | A 'Data.Aeson.Types.String' in
-- <https://www.ietf.org/rfc/rfc3339.txt ISO 8601> format.
--
-- ==== __Examples__
-- >>> schema datetime (String "2000-01-01T00:00:00.000Z")
-- True
datetime :: Schema
datetime =
  SDateTime

-- | An 'Object', possibly with additional fields.
--
-- To match any 'Object', use @'object' []@.
--
-- ==== __Examples__
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1, "bar" .= Bool True]
-- >>> schema (object fields) (Object values)
-- True
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1]]
-- >>> schema (object fields) (Object values)
-- True
object :: [Field] -> Schema
object xs =
  SObject NotStrict (flatten NotStrict xs)

-- | An 'Object' with no additional fields.
--
-- The @'@ mark means \"strict\" as in @foldl'@, because 'object'' matches
-- 'Object's more strictly than 'object'.
--
-- ==== __Examples__
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1]
-- >>> schema (object' fields) (Object values)
-- True
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1, "bar" .= Bool True]
-- >>> schema (object' fields) (Object values)
-- False
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1]]
-- >>> schema (object' fields) (Object values)
-- True
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1], "baz" .= Bool True]
-- >>> schema (object' fields) (Object values)
-- False
object' :: [Field] -> Schema
object' xs =
  SObject Strict (flatten Strict xs)


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


--------------------------------------------------------------------------------
-- Uniplate stuff, not enough to be worth the dependency
--------------------------------------------------------------------------------

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
transform f =
  f . go (transform f)
  where
    -- Transform direct children.
    go :: (Schema -> Schema) -> Schema -> Schema
    go f = \case
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
