{-# LANGUAGE BangPatterns #-}

-- | JSON schema validation.

module Data.Aeson.Validation
  ( -- * Schema validation
    Schema
  , schema
    -- * Boolean schemas
  , bool
  , true
  , false
    -- * Number schemas
  , number
  , integer
  , someNumber
  , someInteger
    -- * String schemas
  , string
  , theString
  , someString
  , regex
    -- * Object schemas
  , Field
  , object
  , object'
  , (.:)
  , (.:?)
    -- * Array schemas (homogenous lists)
  , array
  , sizedArray
  , -- * Set schemas (homogenous, unique lists)
    set
  , sizedSet
    -- * Tuple schemas (heterogeneous lists)
  , tuple
    -- * Miscellaneous schemas
  , anything
  , nullable
  ) where

import Data.Aeson            (Value(..))
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Scientific
import Data.Semigroup
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)
import Data.Vector           (Vector)
import GHC.Exts              (IsList(..), IsString(..))
import Lens.Micro            hiding (set)
import Text.Regex.PCRE.Light (Regex)

import qualified Data.HashMap.Strict   as HashMap
import qualified Data.HashSet          as HashSet
import qualified Data.Vector           as Vector
import qualified Text.Regex.PCRE.Light as Regex

-- $setup
-- >>> import Test.QuickCheck.Instances ()
-- >>> import qualified Data.Text as Text

-- | An opaque object 'Field'.
--
-- Create a 'Field' with '.:' or '.:?', and bundled into a 'Schema' using
-- 'object' or 'object''
data Field
  = ReqField !Text Schema
  | OptField !Text Schema

-- | An opaque JSON 'Schema'.
data Schema
  = SBool
  | STrue
  | SFalse
  | SNumber (Scientific -> Bool)
  | SString (Text -> Bool)
  | SObject !Strict [Field]
  | SArray !Unique !Int {- min len -} !Int {- max len -} Schema
  | STuple [Schema]
  | SAnything
  | SNullable Schema
  | SAlt Schema Schema
  | SInverse Schema

-- | The '<>' operator is used to create a /sum/ 'Schema' that, when applied to
-- a 'Value', first tries the left 'Schema', then falls back on the right one if
-- the left one fails.
--
-- @
-- 'schema' (s1 '<>' s2) val = 'schema' s1 val '||' 'schema' s2 val
-- @
--
-- >>> schema (bool <> string) (Bool True)
-- True
--
-- >>> schema (bool <> string) (String "foo")
-- True
--
-- >>> schema (bool <> string) (Number 1)
-- False
instance Semigroup Schema where
  (<>) = SAlt

-- | The 'Num' instance only defines two functions; all other 'Num' functions
-- call 'error'.
--
--     (1) 'fromInteger' is provided for integer-literal syntax.
--
--         @
--         'fromInteger' n = 'someInteger' ('==' n)
--         @
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
--         >>> schema (negate bool) (Bool True)
--         False
--
--         >>> schema (negate bool) (String "foo")
--         True
--
instance Num Schema where
  (+)    = error "Data.Aeson.Validation: (+) not implemented for Schema"
  (-)    = error "Data.Aeson.Validation: (-) not implemented for Schema"
  (*)    = error "Data.Aeson.Validation: (*) not implemented for Schema"
  abs    = error "Data.Aeson.Validation: abs not implemented for Schema"
  signum = error "Data.Aeson.Validation: signum not implemented for Schema"

  fromInteger n = someInteger (== n)
  negate = SInverse

-- | 'fromString' is provided for string-literal syntax.
--
-- @
-- 'fromString' = 'theString' . 'Data.Text.pack'
-- @
--
-- >>> schema "foo" (String "foo")
-- True
instance IsString Schema where
  fromString = theString . fromString

-- | Are extra properties of an object allowed?
data Strict
  = Strict
  | NotStrict

-- | Are duplicate elements in an array allowed?
data Unique
  = Unique
  | NotUnique

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
bool = SBool

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
true = STrue

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
false = SFalse

-- | Any 'Number'.
--
-- ==== __Examples__
--
-- >>> schema number (Number 1.0)
-- True
number :: Schema
number = SNumber (const True)

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
integer = SNumber isInteger

-- | Some 'Number'.
--
-- ==== __Examples__
--
-- >>> schema (someNumber (> 5)) (Number 6)
-- True
someNumber :: (Scientific -> Bool) -> Schema
someNumber = SNumber

-- | Some integer 'Number'.
--
-- ==== __Examples__
--
-- >>> schema (someInteger (> 5)) (Number 6.0)
-- True
--
-- >>> schema (someInteger (> 5)) (Number 6.5)
-- False
someInteger :: (Integer -> Bool) -> Schema
someInteger p = SNumber (either nope p . floatingOrInteger)
 where
  nope :: Double -> Bool
  nope _ = False

-- | Any 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> schema string (String "foo")
-- True
string :: Schema
string = SString (const True)

-- | An exact 'Data.Aeson.Types.String'. This is what the @OverloadedStrings@
-- instance uses when making a 'Schema' from a string literal.
--
-- ==== __Examples__
--
-- >>> schema (theString "foo") (String "foo")
-- True
--
-- >>> schema (theString "foo") (String "bar")
-- False
theString :: Text -> Schema
theString s = SString (== s)

-- | Some 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> schema (someString (\s -> Text.length s > 5)) (String "foobar")
-- True
--
-- >>> schema (someString (\s -> Text.length s > 5)) (String "foo")
-- False
someString :: (Text -> Bool) -> Schema
someString = SString

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
regex r = SString (\s -> has (_Just . _head) (Regex.match r' (encodeUtf8 s) []))
 where
  r' :: Regex
  r' = Regex.compile (encodeUtf8 r) [Regex.utf8]


-- | An 'Object', possibly with additional unvalidated fields.
--
-- To match any 'Object', use @'object' []@.
object :: [Field] -> Schema
object = SObject NotStrict

-- | An 'Object' with no additional fields.
--
-- The @'@ mark means \"strict\" as in @foldl'@, because 'object'' matches
-- 'Object's more strictly than 'object'.
object' :: [Field] -> Schema
object' = SObject Strict

-- | A required 'Field'.
(.:) :: Text -> Schema -> Field
(.:) = ReqField
infixr 5 .:

-- | An optional 'Field'.
(.:?) :: Text -> Schema -> Field
(.:?) = OptField
infixr 5 .:?

-- | A "homogenous" 'Array' of any size.
--
-- The array need not be /truly/ homogenous; it simply has the same 'Schema'
-- applied to each element. However, the 'Schema' could be 'anything', or
-- composed of many alternatives using '<>'.
--
-- ==== __Examples__
--
-- >>> schema (array bool) (Array [Bool True, Bool False])
-- True
--
-- >>> schema (array anything) (Array [Bool True, String "foo"])
-- True
--
-- >>> schema (array integer) (Array [Number 1.5])
-- False
array :: Schema -> Schema
array = SArray NotUnique minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above) 'Array'. Use 'minBound'
-- or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> schema (sizedArray 1 2 bool) (Array [Bool True])
-- True
--
-- >>> schema (sizedArray 1 2 bool) (Array [Bool True, Bool True, Bool False])
-- False
sizedArray :: Int -> Int -> Schema -> Schema
sizedArray = SArray NotUnique

-- | A "homogenous" (see note above), unique 'Array' of any size.
--
-- ==== __Examples__
--
-- >>> schema (set bool) (Array [Bool True])
-- True
--
-- >>> schema (set bool) (Array [Bool True, Bool True])
-- False
set :: Schema -> Schema
set = SArray Unique minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above), unique 'Array'. Use
-- 'minBound' or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> schema (sizedSet 1 1 string) (Array [String "foo"])
-- True
--
-- >>> schema (sizedSet 1 1 string) (Array [String "foo", String "bar"])
-- False
sizedSet :: Int -> Int -> Schema -> Schema
sizedSet = SArray Unique

-- | A heterogeneous 'Array' exactly as long as the given list of 'Schema's.
--
-- ==== __Examples__
--
-- >>> schema (tuple [bool, string]) (Array [Bool True, String "foo"])
-- True
tuple :: [Schema] -> Schema
tuple = STuple

-- | Any 'Value' whatsoever, including 'Null'.
--
-- ==== __Examples__
--
-- >>> schema anything Null
-- True
--
-- >>> schema anything (Bool True)
-- True
anything :: Schema
anything = SAnything

-- | Modify a 'Schema' to additionally accept 'Null'.
--
-- 'nullable' is idempotent:
--
-- @
-- 'nullable' = 'nullable' . 'nullable'
-- @
--
-- ==== __Examples__
--
-- >>> schema (nullable bool) (Bool True)
-- True
--
-- >>> schema (nullable bool) Null
-- True
nullable :: Schema -> Schema
nullable = SNullable

-- | Does the 'Value' satisfy the 'Schema'?
schema :: Schema -> Value -> Bool
schema SBool (Bool _) = True
schema STrue (Bool True) = True
schema SFalse (Bool False) = True
schema (SNumber f) (Number x) = f x
schema (SString f) (String x) = f x
schema (SObject NotStrict fs) (Object o) = schemaObject fs o
schema (SObject Strict fs) (Object o) = schemaObject' fs o
schema (SArray NotUnique x y SAnything) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y
schema (SArray NotUnique x y s) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (schema s) (toList v)
schema (SArray Unique x y SAnything) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && len == length (toSet v)
schema (SArray Unique x y s) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (schema s) (toList v) &&
       len == length (toSet v)
schema (STuple ss) (Array v) = schemaTuple ss (toList v)
schema SAnything _ = True
schema (SNullable _) Null = True
schema (SNullable s) v = schema s v
schema (SAlt s1 s2) v = schema s1 v || schema s2 v
schema (SInverse s) v = not (schema s v)
schema _ _ = False

schemaObject :: [Field] -> HashMap Text Value -> Bool
schemaObject [] _ = True
schemaObject (ReqField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> False
    Just val -> schema s val && schemaObject xs obj
schemaObject (OptField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> schemaObject xs obj
    Just val -> schema s val && schemaObject xs obj

schemaObject' :: [Field] -> HashMap Text Value -> Bool
schemaObject' [] obj = HashMap.null obj
schemaObject' (ReqField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> False
    Just val -> schema s val && schemaObject xs (HashMap.delete key obj)
schemaObject' (OptField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> schemaObject xs obj
    Just val -> schema s val && schemaObject xs (HashMap.delete key obj)

schemaTuple :: [Schema] -> [Value] -> Bool
schemaTuple [] [] = True
schemaTuple (s:ss) (v:vs) = schema s v && schemaTuple ss vs
schemaTuple _ _ = False

toSet :: Vector Value -> HashSet Value
toSet = Vector.foldr' HashSet.insert mempty
