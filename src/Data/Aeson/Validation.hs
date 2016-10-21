{-# LANGUAGE BangPatterns #-}

module Data.Aeson.Validation
  ( -- * Schema validation
    Schema
  , Field
  , satisfies
    -- * Boolean schemas
  , bool
  , true
  , false
    -- * Number schemas
  , number
  , integer
  , float
  , someNumber
  , someInteger
  , someFloat
    -- * String schemas
  , string
  , theString
  , someString
  , regex
    -- * Object schemas
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
  , nullable
  , anything
  ) where

import Data.Aeson            (Value)
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

import qualified Data.Aeson            as A
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.HashSet          as HashSet
import qualified Data.Vector           as Vector
import qualified Text.Regex.PCRE.Light as Regex

data Field
  = ReqField !Text Schema
  | OptField !Text Schema

data Schema
  = Bool (Bool -> Bool)
  | Number (Scientific -> Bool)
  | String (Text -> Bool)
  | Object !Bool {- strict? -} [Field]
  | Array !Bool {- unique? -} !Int {- min len -} !Int {- max len -} Schema
  | Tuple [Schema]
  | Nullable Schema
  | Alt Schema Schema
  | Anything

instance Semigroup Schema where
  (<>) = Alt

-- | Any 'A.Bool'.
bool :: Schema
bool = Bool (const True)

-- | 'A.Bool' 'True'.
true :: Schema
true = Bool (== True)

-- | 'A.Bool' 'False'.
false :: Schema
false = Bool (== False)

-- | Any 'A.Number'.
number :: Schema
number = Number (const True)

-- | Any integer 'A.Number'.
integer :: Schema
integer = Number isInteger

-- | Any floating 'A.Number'
float :: Schema
float = Number isFloating

-- | Some 'A.Number'.
someNumber :: (Scientific -> Bool) -> Schema
someNumber = Number

-- | Some integer 'A.Number'.
someInteger :: (Integer -> Bool) -> Schema
someInteger p = Number (either nope p . floatingOrInteger)
 where
  nope :: Double -> Bool
  nope _ = False

-- | Some floating 'A.Number'.
someFloat :: RealFloat a => (a -> Bool) -> Schema
someFloat p = Number (p . toRealFloat)

-- | Any 'A.String'.
string :: Schema
string = String (const True)

-- | An exact 'A.String'.
theString :: Text -> Schema
theString s = String (== s)

-- | Some 'A.String'.
someString :: (Text -> Bool) -> Schema
someString = String

regex :: Text -> Schema
regex r = String (\s -> has (_Just . _head) (Regex.match r' (encodeUtf8 s) []))
 where
  r' :: Regex
  r' = Regex.compile (encodeUtf8 r) [Regex.utf8]


-- | An 'A.Object', possibly with additional unvalidated fields.
--
-- To match any 'A.Object', use @'object' []@.
object :: [Field] -> Schema
object = Object False

-- | An 'A.Object' with no additional fields.
--
-- The @\'@ mark means \"strict\" as in @foldl'@, because 'object'' matches
-- 'A.Object's more strictly than 'object'.
object' :: [Field] -> Schema
object' = Object True

-- | A required 'Field'.
(.:) :: Text -> Schema -> Field
(.:) = ReqField
infixr .:

-- | An optional 'Field'.
(.:?) :: Text -> Schema -> Field
(.:?) = OptField
infixr .:?

-- | A "homogenous" 'A.Array' of any size.
--
-- The array need not be _truly_ homogenous; it simply has the same 'Schema'
-- applied to each element. However, the 'Schema' could be composed of many
-- alternatives using '<>'.
array :: Schema -> Schema
array = Array False minBound maxBound

-- | A sized (inclusive), "homogenous" 'A.Array'. Use 'minBound' or 'maxBound'
-- for an (effectively) unbounded edge.
sizedArray :: Int -> Int -> Schema -> Schema
sizedArray = Array False

-- | A "homogenous", unique 'A.Array' of any size.
set :: Schema -> Schema
set = Array True minBound maxBound

-- | A sized (inclusive), "homogenous", unique 'A.Array'. Use 'minBound' or
-- 'maxBound' for an (effectively) unbounded edge.
sizedSet :: Int -> Int -> Schema -> Schema
sizedSet = Array True

-- | A heterogeneous 'A.Array' exactly as long as the given list of 'Schema's.
tuple :: [Schema] -> Schema
tuple = Tuple

-- | Modify a 'Schema' to additionally accept 'A.Null'.
--
-- 'nullable' is idempotent:
--
-- @
-- 'nullable' a = 'nullable' ('nullable' a)
-- @
nullable :: Schema -> Schema
nullable = Nullable

-- | Any 'A.Value' whatsoever.
anything :: Schema
anything = Anything

-- | Does the 'A.Value' satisfy the 'Schema'?
satisfies :: Schema -> Value -> Bool
satisfies (Bool f) (A.Bool x) = f x
satisfies (Number f) (A.Number x) = f x
satisfies (String f) (A.String x) = f x
satisfies (Object False fs) (A.Object o) = satisfiesObject fs o
satisfies (Object True fs) (A.Object o) = satisfiesObject' fs o
satisfies (Array False x y Anything) (A.Array v) =
  let !len = Vector.length v
  in len >= x && len <= y
satisfies (Array False x y s) (A.Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (satisfies s) (toList v)
satisfies (Array True x y Anything) (A.Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && len == length (toSet v)
satisfies (Array True x y s) (A.Array v) =
  let !len = Vector.length v
  in len >= x && len <= y && all (satisfies s) (toList v) &&
       len == length (toSet v)
satisfies (Tuple ss) (A.Array v) = satisfiesTuple ss (toList v)
satisfies (Nullable _) A.Null = True
satisfies (Nullable s) v = satisfies s v
satisfies (Alt s1 s2) v = satisfies s1 v || satisfies s2 v
satisfies Anything _ = True
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
