{-# LANGUAGE BangPatterns #-}

module Data.Aeson.Validation
  ( -- * Schema validation
    Schema
  , satisfies
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
  , inverse
  ) where

import Data.Aeson            (Value(..))
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Scientific
import Data.Semigroup
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)
import Data.Vector           (Vector)
import GHC.Exts              (IsList(..))
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
--
-- The '<>' operator is used to create a /sum/ 'Schema' that, when applied to a
-- 'Value', first tries the left 'Schema', then falls back on the right one if
-- the left one fails.
data Schema
  = SBool (Bool -> Bool)
  | SNumber (Scientific -> Bool)
  | SString (Text -> Bool)
  | SObject !Bool {- strict? -} [Field]
  | SArray !Bool {- unique? -} !Int {- min len -} !Int {- max len -} Schema
  | STuple [Schema]
  | SAnything
  | SNullable Schema
  | SAlt Schema Schema
  | SInverse Schema

instance Semigroup Schema where
  (<>) = SAlt

-- | Any 'Data.Aeson.Types.Bool'.
--
-- ==== __Examples__
--
-- >>> satisfies bool (Bool True)
-- True
--
-- >>> satisfies bool (Bool False)
-- True
bool :: Schema
bool = SBool (const True)

-- | 'Data.Aeson.Types.Bool' 'True'.
--
-- ==== __Examples__
--
-- >>> satisfies true (Bool True)
-- True
--
-- >>> satisfies true (Bool False)
-- False
true :: Schema
true = SBool (== True)

-- | 'Data.Aeson.Types.Bool' 'False'.
--
-- ==== __Examples__
--
-- >>> satisfies false (Bool True)
-- False
--
-- >>> satisfies false (Bool False)
-- True
false :: Schema
false = SBool (== False)

-- | Any 'Number'.
--
-- ==== __Examples__
--
-- >>> satisfies number (Number 1.0)
-- True
number :: Schema
number = SNumber (const True)

-- | Any integer 'Number'.
--
-- ==== __Examples__
--
-- >>> satisfies integer (Number 1.0)
-- True
--
-- >>> satisfies integer (Number 1.5)
-- False
integer :: Schema
integer = SNumber isInteger

-- | Some 'Number'.
--
-- ==== __Examples__
--
-- >>> satisfies (someNumber (> 5)) (Number 6)
-- True
someNumber :: (Scientific -> Bool) -> Schema
someNumber = SNumber

-- | Some integer 'Number'.
--
-- ==== __Examples__
--
-- >>> satisfies (someInteger (> 5)) (Number 6.0)
-- True
--
-- >>> satisfies (someInteger (> 5)) (Number 6.5)
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
-- >>> satisfies string (String "foo")
-- True
string :: Schema
string = SString (const True)

-- | An exact 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> satisfies (theString "foo") (String "foo")
-- True
--
-- >>> satisfies (theString "foo") (String "bar")
-- False
theString :: Text -> Schema
theString s = SString (== s)

-- | Some 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> satisfies (someString (\s -> Text.length s > 5)) (String "foobar")
-- True
--
-- >>> satisfies (someString (\s -> Text.length s > 5)) (String "foo")
-- False
someString :: (Text -> Bool) -> Schema
someString = SString

-- | A 'Data.Aeson.Types.String' that matches a regular expression.
--
-- ==== __Examples__
--
-- >>> satisfies (regex "a+b") (String "xaaabx")
-- True
--
-- >>> satisfies (regex "c{2}") (String "cd")
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
object = SObject False

-- | An 'Object' with no additional fields.
--
-- The @'@ mark means \"strict\" as in @foldl'@, because 'object'' matches
-- 'Object's more strictly than 'object'.
object' :: [Field] -> Schema
object' = SObject True

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
-- >>> satisfies (array bool) (Array [Bool True, Bool False])
-- True
--
-- >>> satisfies (array anything) (Array [Bool True, String "foo"])
-- True
--
-- >>> satisfies (array integer) (Array [Number 1.5])
-- False
array :: Schema -> Schema
array = SArray False minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above) 'Array'. Use 'minBound'
-- or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> satisfies (sizedArray 1 2 bool) (Array [Bool True])
-- True
--
-- >>> satisfies (sizedArray 1 2 bool) (Array [Bool True, Bool True, Bool False])
-- False
sizedArray :: Int -> Int -> Schema -> Schema
sizedArray = SArray False

-- | A "homogenous" (see note above), unique 'Array' of any size.
--
-- ==== __Examples__
--
-- >>> satisfies (set bool) (Array [Bool True])
-- True
--
-- >>> satisfies (set bool) (Array [Bool True, Bool True])
-- False
set :: Schema -> Schema
set = SArray True minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above), unique 'Array'. Use
-- 'minBound' or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> satisfies (sizedSet 1 1 string) (Array [String "foo"])
-- True
--
-- >>> satisfies (sizedSet 1 1 string) (Array [String "foo", String "bar"])
-- False
sizedSet :: Int -> Int -> Schema -> Schema
sizedSet = SArray True

-- | A heterogeneous 'Array' exactly as long as the given list of 'Schema's.
--
-- ==== __Examples__
--
-- >>> satisfies (tuple [bool, string]) (Array [Bool True, String "foo"])
-- True
tuple :: [Schema] -> Schema
tuple = STuple

-- | Any 'Value' whatsoever, including 'Null'.
--
-- ==== __Examples__
--
-- >>> satisfies anything Null
-- True
--
-- >>> satisfies anything (Bool True)
-- True
anything :: Schema
anything = SAnything

-- | Modify a 'Schema' to additionally accept 'Null'.
--
-- 'nullable' is idempotent:
--
-- @
-- 'nullable' a = 'nullable' ('nullable' a)
-- @
--
-- ==== __Examples__
--
-- >>> satisfies (nullable bool) (Bool True)
-- True
--
-- >>> satisfies (nullable bool) Null
-- True
nullable :: Schema -> Schema
nullable = SNullable

-- | Succeed whenever the given 'Schema' fails, and vice versa.
--
-- @
-- 'inverse' '.' 'inverse' = 'id'
-- @
--
-- ==== __Examples__
--
-- >>> satisfies (inverse bool) (Bool True)
-- False
--
-- >>> satisfies (inverse bool) (String "foo")
-- True
inverse :: Schema -> Schema
inverse = SInverse

-- | Does the 'Value' satisfy the 'Schema'?
satisfies :: Schema -> Value -> Bool
satisfies (SBool f) (Bool x) = f x
satisfies (SNumber f) (Number x) = f x
satisfies (SString f) (String x) = f x
satisfies (SObject False fs) (Object o) = satisfiesObject fs o
satisfies (SObject True fs) (Object o) = satisfiesObject' fs o
satisfies (SArray False x y SAnything) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y
satisfies (SArray False x y s) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (satisfies s) (toList v)
satisfies (SArray True x y SAnything) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && len == length (toSet v)
satisfies (SArray True x y s) (Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (satisfies s) (toList v) &&
       len == length (toSet v)
satisfies (STuple ss) (Array v) = satisfiesTuple ss (toList v)
satisfies SAnything _ = True
satisfies (SNullable _) Null = True
satisfies (SNullable s) v = satisfies s v
satisfies (SAlt s1 s2) v = satisfies s1 v || satisfies s2 v
satisfies (SInverse s) v = not (satisfies s v)
satisfies _ _ = False

satisfiesObject :: [Field] -> HashMap Text Value -> Bool
satisfiesObject [] _ = True
satisfiesObject (ReqField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> False
    Just val -> satisfies s val && satisfiesObject xs obj
satisfiesObject (OptField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> satisfiesObject xs obj
    Just val -> satisfies s val && satisfiesObject xs obj

satisfiesObject' :: [Field] -> HashMap Text Value -> Bool
satisfiesObject' [] obj = HashMap.null obj
satisfiesObject' (ReqField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> False
    Just val -> satisfies s val && satisfiesObject xs (HashMap.delete key obj)
satisfiesObject' (OptField key s : xs) obj =
  case HashMap.lookup key obj of
    Nothing  -> satisfiesObject xs obj
    Just val -> satisfies s val && satisfiesObject xs (HashMap.delete key obj)

satisfiesTuple :: [Schema] -> [Value] -> Bool
satisfiesTuple [] [] = True
satisfiesTuple (s:ss) (v:vs) = satisfies s v && satisfiesTuple ss vs
satisfiesTuple _ _ = False

toSet :: Vector Value -> HashSet Value
toSet = Vector.foldr' HashSet.insert mempty
