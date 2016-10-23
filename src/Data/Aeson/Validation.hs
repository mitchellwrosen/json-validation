{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | JSON schema validation.

module Data.Aeson.Validation
  ( -- * Schema validation
    Schema
  , schema
  , validate
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

import Data.Aeson.Validation.Types

import Control.Monad.Reader
import Control.Monad.Writer  hiding ((<>))
import Data.Aeson            (Value(..))
import Data.Foldable
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Scientific
import Data.Semigroup
import Data.Sequence         (Seq)
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)
import Data.Vector           (Vector)
import Lens.Micro            hiding (set)
import Text.Regex.PCRE.Light (Regex)

import qualified Data.HashMap.Strict   as HashMap
import qualified Data.HashSet          as HashSet
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector
import qualified GHC.Exts              as GHC
import qualified Text.Regex.PCRE.Light as Regex

-- $setup
-- >>> import Test.QuickCheck.Instances ()
-- >>> import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Instances

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
instance Num Schema where
  (+)    = error "Data.Aeson.Validation: (+) not implemented for Schema"
  (-)    = error "Data.Aeson.Validation: (-) not implemented for Schema"
  (*)    = error "Data.Aeson.Validation: (*) not implemented for Schema"
  abs    = error "Data.Aeson.Validation: abs not implemented for Schema"
  signum = error "Data.Aeson.Validation: signum not implemented for Schema"

  fromInteger n = someInteger (== n)
  negate = SNegate

-- | 'fromString' is provided for string-literal syntax.
--
-- @
-- 'fromString' = 'theString' . 'Data.Text.pack'
-- @
--
-- >>> schema "foo" (String "foo")
-- True
instance GHC.IsString Schema where
  fromString = theString . GHC.fromString

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

--------------------------------------------------------------------------------
-- Schemas

class A schema where
  -- | Some 'Number'.
  --
  -- The unexported @A@ typeclass exists to overload 'someNumber' with precicely
  -- two possible types:
  --
  -- @
  -- 'someNumber' :: ('Scientific' -> 'Bool') -> 'Schema'
  -- 'someNumber' :: ('Scientific' -> 'Bool') -> 'Text' -> 'Schema'
  -- @
  --
  -- The optional 'Text' argument is used for error reporting if validation
  -- fails.
  --
  -- ==== __Examples__
  --
  -- >>> schema (someNumber (> 5)) (Number 6)
  -- True
  someNumber :: (Scientific -> Bool) -> schema

  -- | Some integer 'Number'.
  --
  -- The unexported @A@ typeclass exists to overload 'someInteger' with
  -- precicely two possible types:
  --
  -- @
  -- 'someInteger' :: ('Integer' -> 'Bool') -> 'Schema'
  -- 'someInteger' :: ('Integer' -> 'Bool') -> 'Text' -> 'Schema'
  -- @
  --
  -- The optional 'Text' argument is used for error reporting if validation
  -- fails.
  --
  -- ==== __Examples__
  --
  -- >>> schema (someInteger (> 5)) (Number 6.0)
  -- True
  --
  -- >>> schema (someInteger (> 5)) (Number 6.5)
  -- False
  someInteger :: (Integer -> Bool) -> schema

  -- | Some 'Data.Aeson.Types.String'.
  --
  -- The unexported @A@ typeclass exists to overload 'someString' with precicely
  -- two possible types:
  --
  -- @
  -- 'someString' :: ('Text' -> 'Bool') -> 'Schema'
  -- 'someString' :: ('Text' -> 'Bool') -> 'Text' -> 'Schema'
  -- @
  --
  -- The optional 'Text' argument is used for error reporting if validation
  -- fails.
  --
  -- ==== __Examples__
  --
  -- >>> schema (someString (\s -> Text.length s > 5)) (String "foobar")
  -- True
  --
  -- >>> schema (someString (\s -> Text.length s > 5)) (String "foo")
  -- False
  someString :: (Text -> Bool) -> schema

instance A Schema where
  someNumber :: (Scientific -> Bool) -> Schema
  someNumber p = SSomeNumber p Nothing

  someInteger :: (Integer -> Bool) -> Schema
  someInteger p = SSomeNumber (either nope p . floatingOrInteger) Nothing
   where
    nope :: Double -> Bool
    nope _ = False

  someString :: (Text -> Bool) -> Schema
  someString p = SSomeString p Nothing

instance (a ~ Text) => A (a -> Schema) where
  someNumber :: (Scientific -> Bool) -> Text -> Schema
  someNumber p s = SSomeNumber p (Just s)

  someInteger :: (Integer -> Bool) -> Text -> Schema
  someInteger p s =
    SSomeNumber (either nope p . floatingOrInteger) (Just s)
   where
    nope :: Double -> Bool
    nope _ = False

  someString :: (Text -> Bool) -> Text -> Schema
  someString p s = SSomeString p (Just s)

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
number = SNumber

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
integer = SInteger

-- | Any 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> schema string (String "foo")
-- True
string :: Schema
string = SString

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
theString = STheString

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
regex r = SSomeString p (Just ("matches " <> r))
 where
  p :: Text -> Bool
  p s = has (_Just . _head) (Regex.match r' (encodeUtf8 s) [])

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

--------------------------------------------------------------------------------
-- Validation

-- | Does the 'Value' satisfy the 'Schema'?
--
-- @
-- 'schema' s v = 'null' ('validate' s v)
-- @
schema :: Schema -> Value -> Bool
schema s v = null (validate s v)

-- | Validate a 'Value' with a 'Schema' and emit schema violations as 'Text'.
validate :: Schema -> Value -> [Text]
validate s v = toList (execWriter (runReaderT (validate_ s v) Empty))

validate_ :: Schema -> Value -> Validation ()
validate_ = \case
  SBool           -> validateBool
  STrue           -> validateTrue
  SFalse          -> validateFalse
  SNumber         -> validateNumber
  SInteger        -> validateInteger
  SSomeNumber p s -> validateSomeNumber p s
  SString         -> validateString
  STheString s    -> validateTheString s
  SSomeString p s -> validateSomeString p s
  SObject s xs    -> validateObject s xs
  SArray u x y s  -> validateArray u x y s
  STuple ss       -> validateTuple ss
  SAnything       -> const ok
  SNullable s     -> validateNullable s
  SAlt s1 s2      -> validateAlt s1 s2
  SNegate s0      ->
    case s0 of
      SBool           -> validateNotBool
      STrue           -> validateNotTrue
      SFalse          -> validateNotFalse
      SNumber         -> validateNotNumber
      SInteger        -> validateNotInteger
      SSomeNumber p s -> validateNotSomeNumber p s
      SString         -> validateNotString
      STheString s    -> validateNotTheString s
      SSomeString p s -> validateNotSomeString p s
      SObject s xs    -> validateNotObject s xs
      SArray u x y s  -> validateNotArray u x y s
      STuple ss       -> validateNotTuple ss
      SAnything       -> const (err "negate anything")
      SNullable s     -> validateNotNullable s
      SAlt s1 s2      -> validateNotAlt s1 s2
      SNegate s       -> validate_ s

validateBool :: Value -> Validation ()
validateBool = \case
  Bool _ -> ok
  v      -> mismatch "a bool" (valType v)

validateTrue :: Value -> Validation ()
validateTrue = \case
  Bool b -> unless b (mismatch "true" "false")
  v      -> mismatch "true" (valType v)

validateFalse :: Value -> Validation ()
validateFalse = \case
  Bool b -> when b (mismatch "false" "true")
  v      -> mismatch "false" (valType v)

validateNumber :: Value -> Validation ()
validateNumber = \case
  Number _ -> ok
  v        -> mismatch "a number" (valType v)

validateInteger :: Value -> Validation ()
validateInteger = \case
  Number n -> unless (isInteger n) (mismatch "an integer" (tshow n))
  v        -> mismatch "an integer" (valType v)

validateSomeNumber
  :: (Scientific -> Bool) -> Maybe Text -> Value -> Validation ()
validateSomeNumber p msg = \case
  Number n -> unless (p n) (failedPredicate msg)
  v        -> mismatch "a number" (valType v)

validateString :: Value -> Validation ()
validateString = \case
  String _ -> ok
  v        -> mismatch "a string" (valType v)

validateTheString :: Text -> Value -> Validation ()
validateTheString s = \case
  String s' -> unless (s == s') (mismatch (tshow s) (tshow s'))
  v         -> mismatch (tshow s) (valType v)

validateSomeString :: (Text -> Bool) -> Maybe Text -> Value -> Validation ()
validateSomeString p msg = \case
  String s -> unless (p s) (failedPredicate msg)
  v        -> mismatch "a string" (valType v)

validateObject :: Strict -> [Field] -> Value -> Validation ()
validateObject s xs = \case
  Object obj ->
    case s of
      NotStrict -> mapM_ (validateField obj) xs
      Strict    -> validateObject' xs obj
  v -> mismatch "an object" (valType v)

validateObject' :: [Field] -> HashMap Text Value -> Validation ()
validateObject' [] obj =
  case HashMap.keys obj of
    [] -> ok
    ks -> err ("extra fields: " <> Text.intercalate ", " ks)
validateObject' (x:xs) obj = do
  validateField obj x
  validateObject' xs (HashMap.delete (fieldKey x) obj)
 where
  fieldKey :: Field -> Text
  fieldKey (ReqField s _) = s
  fieldKey (OptField s _) = s

validateField :: HashMap Text Value -> Field -> Validation ()
validateField obj = \case
  ReqField key s ->
    case HashMap.lookup key obj of
      Nothing -> err ("missing field" <> key)
      Just v  -> local (Property key) (validate_ s v)
  OptField key s ->
    case HashMap.lookup key obj of
      Nothing -> ok
      Just v  -> local (Property key) (validate_ s v)

validateArray :: Unique -> Int -> Int -> Schema -> Value -> Validation ()
validateArray uniq x y sch = \case
  Array v -> do
    case sch of
      SAnything -> ok
      _ -> Vector.imapM_ (\n val -> local (Index n) (validate_ sch val)) v
    when (uniq == Unique) (uniqCheck v)
    boundsCheck (length v)
  v -> mismatch "an array" (valType v)
 where
  boundsCheck :: Int -> Validation ()
  boundsCheck len =
    unless (len >= x && len <= y) $
      case (x == minBound, y == maxBound) of
        (True, True) -> error "impossible"
        (True, False) -> mismatch ("<= " <> tshow y <> " elements") (tshow len)
        (False, True) -> mismatch (">= " <> tshow x <> " elements") (tshow len)
        (False, False) ->
          mismatch
            ("between " <> tshow x <> " and " <> tshow y <>
              " elements (inclusive)")
            (tshow len)

  uniqCheck :: Vector Value -> Validation ()
  uniqCheck v =
    when (length v /= length (toSet v))
      (err "array does not contain unique elements")
   where
    toSet :: Vector Value -> HashSet Value
    toSet = Vector.foldr' HashSet.insert mempty


validateTuple :: [Schema] -> Value -> Validation ()
validateTuple ss0 = \case
  Array v0 -> go 0 ss0 (toList v0)
   where
    go :: Int -> [Schema] -> [Value] -> Validation ()
    go _  [] [] = ok
    go !n (s:ss) (v:vs) = do
      local (Index n) (validate_ s v)
      go (n+1) ss vs
    go _ _ _ =
      mismatch (tshow (length ss0) <> " elements")
        (tshow (length v0) <> (" elements"))
  v -> mismatch "an array" (valType v)

validateNullable :: Schema -> Value -> Validation ()
validateNullable s v = when (v /= Null) (validate_ s v)

validateAlt :: Schema -> Schema -> Value -> Validation ()
validateAlt s1 s2 val = do
  errs1 <- vlisten (validate_ s1 val)
  unless (null errs1) $ do
    errs2 <- vlisten (validate_ s2 val)
    unless (null errs2) (tell (errs1 <> errs2))

validateNotBool :: Value -> Validation ()
validateNotBool v =
  case v of
    Bool b -> mismatch "anything but a bool" (if b then "true" else "false")
    _      -> ok

validateNotTrue :: Value -> Validation ()
validateNotTrue = \case
  Bool True -> mismatch "anything but true" "true"
  _         -> ok

validateNotFalse :: Value -> Validation ()
validateNotFalse = \case
  Bool False -> mismatch "anything but false" "false"
  _          -> ok

validateNotNumber :: Value -> Validation ()
validateNotNumber v =
  case v of
    Number n -> mismatch "anything but a number" (tshow n)
    _        -> ok

validateNotInteger :: Value -> Validation ()
validateNotInteger = \case
  Number n | isInteger n ->
    mismatch "anything but an integer" (tshow m)
   where
    m :: Integer
    m = either nope id (floatingOrInteger n)

    nope :: Double -> Integer
    nope = error "impossible"
  _ -> ok

validateNotSomeNumber
  :: (Scientific -> Bool) -> Maybe Text -> Value -> Validation ()
validateNotSomeNumber p msg = \case
  Number n | p n -> passedPredicate msg
  _ -> ok

validateNotString :: Value -> Validation ()
validateNotString v =
  case v of
    String s -> mismatch "anything but a string" (tshow s)
    _        -> ok

validateNotTheString :: Text -> Value -> Validation ()
validateNotTheString s = \case
  String s' | s == s' -> mismatch ("anything but " <> tshow s) (tshow s)
  _ -> ok

validateNotSomeString :: (Text -> Bool) -> Maybe Text -> Value -> Validation ()
validateNotSomeString p msg = \case
  String s | p s -> passedPredicate msg
  _ -> ok

validateNotObject :: Strict -> [Field] -> Value -> Validation ()
validateNotObject sch xs val = do
  errs <- vlisten (validateObject sch xs val)
  when (null errs) (err "passed object schema")

validateNotArray :: Unique -> Int -> Int -> Schema -> Value -> Validation ()
validateNotArray uniq x y sch val = do
  errs <- vlisten (validateArray uniq x y sch val)
  when (null errs) (err ("passed " <> typ <> " schema"))
 where
  typ =
    case uniq of
      Unique    -> "set"
      NotUnique -> "array"

validateNotTuple :: [Schema] -> Value -> Validation ()
validateNotTuple ss val = do
  errs <- vlisten (validateTuple ss val)
  when (null errs) (err "passed tuple schema")

validateNotNullable :: Schema -> Value -> Validation ()
validateNotNullable s = \case
  Null -> err "passed nullable schema"
  v    -> validate_ (negate s) v

validateNotAlt :: Schema -> Schema -> Value -> Validation ()
validateNotAlt s1 s2 val = do
  validate_ (negate s1) val
  validate_ (negate s2) val

-- Run a 'Validation' action in the current 'Context', capturing everything it
-- tells.
vlisten :: Validation a -> Validation (Seq Text)
vlisten = mapReaderT (pure . execWriter)

ok :: Validation ()
ok = pure ()

-- Validation error; prefixes the message with the current context (if any).
err :: Text -> Validation ()
err msg = do
  ctx <- ask
  tell (pure (prefix ctx <> msg))
 where
  prefix :: Context -> Text
  prefix = maybe mempty (\s -> "in context " <> s <> ", ") . prettyPrintContext

  -- |
  -- >>> prettyPrintContext Empty
  -- Nothing
  --
  -- >>> prettyPrintContext (Property "foo" (Index 1 (Property "bar" Empty)))
  -- Just "\"bar\".1.\"foo\""
  prettyPrintContext :: Context -> Maybe Text
  prettyPrintContext = \case
    Empty            -> Nothing
    Property key ctx -> Just (go ctx <> tshow key)
    Index    idx ctx -> Just (go ctx <> tshow idx)
   where
    go :: Context -> Text
    go = \case
      Empty            -> ""
      Property key ctx -> go ctx <> tshow key <> "."
      Index    idx ctx -> go ctx <> tshow idx <> "."

-- Generic "mismatch" of the expected/actual sort.
mismatch :: Text -> Text -> Validation ()
mismatch x y = err ("expected " <> x <> " but found " <> y)

-- Erroneously failed a (possibly named) predicate.
failedPredicate :: Maybe Text -> Validation ()
failedPredicate msg =
  err (maybe "failed predicate" ("failed predicate: " <>) msg)

-- Erroneously passed a (possibly named) predicate.
passedPredicate :: Maybe Text -> Validation ()
passedPredicate msg =
  err (maybe "passed predicate" ("passed predicate: " <>) msg)

valType :: Value -> Text
valType = \case
  Null     -> "null"
  Bool   _ -> "a bool"
  Number _ -> "a number"
  String _ -> "a string"
  Object _ -> "an object"
  Array  _ -> "an array"

tshow :: Show a => a -> Text
tshow = Text.pack . show
